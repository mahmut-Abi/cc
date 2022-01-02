// Copyright 2021 The CC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package cc // import "modernc.org/cc/v4"

import (
	"bytes"
	"encoding/hex"
	"flag"
	"fmt"
	"io"
	"math"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"runtime/debug"
	"strings"
	"testing"

	"github.com/dustin/go-humanize"
	"github.com/pmezard/go-difflib/difflib"
	"modernc.org/ccorpus"
	"modernc.org/httpfs"
)

var (
	corpus      = map[string][]byte{}
	corpusIndex []string
	re          *regexp.Regexp
	testCfg     = &Config{}

	oTrace = flag.Bool("trc", false, "Print tested paths.")
)

func init() {
	var err error
	if testCfg.Predefined, testCfg.IncludePaths, testCfg.SysIncludePaths, err = HostConfig(""); err != nil {
		panic(errorf("cannot acquire host configuration: %v", err))
	}

	if testCfg.ABI, err = NewABI(runtime.GOOS, runtime.GOARCH); err != nil {
		panic(errorf("cannot configure ABI: %v", err))
	}

	fs := ccorpus.FileSystem()
	var walk func(fs *httpfs.FileSystem, dir string, f func(pth string, fi os.FileInfo) error) error
	walk = func(fs *httpfs.FileSystem, dir string, f func(pth string, fi os.FileInfo) error) error {
		if !strings.HasSuffix(dir, "/") {
			dir += "/"
		}
		root, err := fs.Open(dir)
		if err != nil {
			return err
		}

		fi, err := root.Stat()
		if err != nil {
			return err
		}

		if !fi.IsDir() {
			return fmt.Errorf("%s: not a directory", fi.Name())
		}

		fis, err := root.Readdir(-1)
		if err != nil {
			return err
		}

		for _, v := range fis {
			switch {
			case v.IsDir():
				if err = walk(fs, v.Name(), f); err != nil {
					return err
				}
			default:
				if err = f(v.Name(), v); err != nil {
					return err
				}
			}
		}
		return nil
	}

	var chars int
	if err := walk(ccorpus.FileSystem(), "/", func(pth string, fi os.FileInfo) error {
		if fi.IsDir() {
			return nil
		}

		f, err := fs.Open(pth)
		if err != nil {
			return errorf("%v: %v", pth, err)
		}

		b, err := io.ReadAll(f)
		if err != nil {
			return errorf("%v: %v", pth, err)
		}

		switch filepath.Ext(pth) {
		case ".c", ".h":
			if len(b) != 0 && b[len(b)-1] != '\n' {
				b = append(b, '\n')
			}
		}
		chars += len(b)
		corpus[pth] = b
		corpusIndex = append(corpusIndex, pth)
		return nil
	}); err != nil {
		panic(err)
	}
}

// Produce the AST used in examples documentation.
func exampleAST(rule int, src string) interface{} {
	return "TODO"
	// src = strings.Replace(src, "\\n", "\n", -1)
	// cfg := &Config{ignoreErrors: true, PreprocessOnly: true}
	// ctx := newContext(cfg)
	// ctx.keywords = gccKeywords
	// ast, _ := parse(ctx, nil, nil, []Source{{Name: "example.c", Value: src, DoNotCache: true}})
	// if ast == nil {
	// 	return "FAIL"
	// }

	// pc, _, _, _ := runtime.Caller(1)
	// typ := runtime.FuncForPC(pc - 1).Name()
	// i := strings.LastIndexByte(typ, '.')
	// typ = typ[i+1+len("Example"):]
	// i = strings.LastIndexByte(typ, '_')
	// typ = typ[:i]
	// var node Node
	// depth := mathutil.MaxInt
	// findNode(typ, ast.TranslationUnit, 0, &node, &depth)
	// return node
}

func TestMain(m *testing.M) {
	oRE := flag.String("re", "", "")
	flag.Parse()
	if *oRE != "" {
		re = regexp.MustCompile(*oRE)
	}
	os.Exit(m.Run())
}

func TestScannerSource(t *testing.T) {
	const fn = "all_test.go"
	exp, err := os.ReadFile(fn)
	if err != nil {
		t.Fatal(err)
	}

	f, err := os.Open(fn)
	if err != nil {
		t.Fatal(err)
	}

	testScannerSource(t, fn, f, exp, false)
	testScannerSource(t, fn, exp, exp, false)
	testScannerSource(t, fn, string(exp), exp, false)
	testScannerSource(t, fn, bytes.NewReader(exp), exp, false)
	testScannerSource(t, fn, nil, exp, false)
	testScannerSource(t, fn, 42, nil, true)
}

func testScannerSource(t *testing.T, name string, value interface{}, exp []byte, mustFail bool) {
	ss, err := newScannerSource(Source{name, value})
	if err != nil != mustFail {
		t.Fatalf("(%q, %T): %v", name, value, err)
	}

	if err != nil {
		return
	}

	if !bytes.Equal(ss.buf, exp) {
		t.Fatal("buf does not match")
	}
}

func TestToken(t *testing.T) {
	s, err := newScannerSource(Source{"test", `abc
def
 ghi
`})
	// abc\ndef\n ghi\n
	//             1
	// 0123 4567 89012
	if err != nil {
		t.Fatal(err)
	}

	s.file.AddLine(4)
	s.file.AddLine(8)
	s.file.AddLine(13)
	for itest, test := range []struct {
		Token
		line int
		col  int
		ch   rune
		sep  string
		src  string
	}{
		{newToken(s, 0, 0, 0, 3), 1, 1, 0, "", "abc"},
		{newToken(s, 1, 3, 4, 3), 2, 1, 1, "\n", "def"},
		{newToken(s, 2, 7, 9, 3), 3, 2, 2, "\n ", "ghi"},
		{newToken(s, eof, 13, 13, 0), 3, 6, eof, "", ""},
	} {
		tok := test.Token
		if g, e := tok.Position().Line, test.line; g != e {
			t.Fatal(itest, g, e)
		}
		if g, e := tok.Position().Column, test.col; g != e {
			t.Fatal(itest, g, e)
		}
		if g, e := tok.Ch, test.ch; g != e {
			t.Fatal(itest, g, e)
		}
		if g, e := string(tok.Sep()), test.sep; g != e {
			t.Fatalf("%v %q %q", itest, g, e)
		}
		if g, e := string(tok.Src()), test.src; g != e {
			t.Fatalf("%v %q %q", itest, g, e)
		}

		tok2 := tok
		tok2.Set([]byte("xyz0123"), []byte("456789"))
		if g, e := string(tok.Sep()), test.sep; g != e {
			t.Fatalf("%v %q %q", itest, g, e)
		}
		if g, e := string(tok.Src()), test.src; g != e {
			t.Fatalf("%v %q %q", itest, g, e)
		}
		if g, e := string(tok2.Sep()), "xyz0123"; g != e {
			t.Fatalf("%v %q %q", itest, g, e)
		}
		if g, e := string(tok2.Src()), "456789"; g != e {
			t.Fatalf("%v %q %q", itest, g, e)
		}
	}
}

func TestScanner(t *testing.T) {
	var files, tokens, chars int64
	var m0, m runtime.MemStats
	debug.FreeOSMemory()
	runtime.ReadMemStats(&m0)
	for _, path := range corpusIndex {
		switch filepath.Ext(path) {
		case ".c", ".h":
			buf := corpus[path]
			chars += int64(len(buf))
			var s *scanner
			var err error
			if s, err = newScanner(Source{path, buf}, func(msg string, args ...interface{}) {
				s.close()
				t.Fatalf(msg, args...)
			}); err != nil {
				t.Fatal(path, err)
			}

			files++
			for {
				tok := s.cppScan()
				if tok.Ch == eof {
					break
				}

				tokens++
			}
		}
	}
	runtime.ReadMemStats(&m)
	t.Logf("files %v, tokens %v, bytes %v, heap %v", h(files), h(tokens), h(chars), h(m.HeapAlloc-m0.HeapAlloc))
}

func h(v interface{}) string {
	switch x := v.(type) {
	case int64:
		return humanize.Comma(x)
	case uint64:
		if x <= math.MaxInt64 {
			return humanize.Comma(int64(x))
		}
	}
	return fmt.Sprint(v)
}

func BenchmarkScanner(b *testing.B) {
	debug.FreeOSMemory()
	for i := 0; i < b.N; i++ {
		var chars int64
		for _, path := range corpusIndex {
			switch filepath.Ext(path) {
			case ".c", ".h":
				buf := corpus[path]
				chars += int64(len(buf))
				var s *scanner
				var err error
				if s, err = newScanner(Source{path, buf}, func(msg string, args ...interface{}) {
					s.close()
					b.Fatalf(msg, args...)
				}); err != nil {
					b.Fatal(path, err)
				}
				for {
					tok := s.cppScan()
					if tok.Ch == eof {
						break
					}
				}
			}
		}
		b.SetBytes(chars)
	}
}

var cppParseBlacklist = map[string]struct{}{
	"/github.com/vnmakarov/mir/c-tests/new/endif.c": {}, // 1:1: unexpected #endif
}

func TestCPPParse(t *testing.T) {
	var files, lines, chars int64
	var asts []group
	var m0, m runtime.MemStats
	debug.FreeOSMemory()
	runtime.ReadMemStats(&m0)
	for _, path := range corpusIndex {
		if _, ok := cppParseBlacklist[path]; ok {
			continue
		}

		switch filepath.Ext(path) {
		case ".c", ".h":
			buf := corpus[path]
			chars += int64(len(buf))
			var p *cppParser
			var err error
			if p, err = newCppParser(Source{path, buf}, func(msg string, args ...interface{}) {
				p.close()
				t.Fatalf(msg, args...)
			}); err != nil {
				t.Fatal(path, err)
			}

			files++
			ast := p.preprocessingFile()
			if len(ast) == 0 {
				t.Fatalf("%v: empty AST", path)
			}

			eol := ast[len(ast)-1]
			x, ok := eol.(eofLine)
			if !ok {
				t.Fatalf("%v: AST not terminated: %T", p.pos(), eol)
			}

			eof := Token(x)
			lines += int64(eof.Position().Line)
			asts = append(asts, ast)
		}
	}
	runtime.ReadMemStats(&m)
	t.Logf("files %v, lines %v, bytes %v, heap %v", h(files), h(lines), h(chars), h(m.HeapAlloc-m0.HeapAlloc))
}

func BenchmarkCPPParse(b *testing.B) {
	debug.FreeOSMemory()
	for i := 0; i < b.N; i++ {
		var chars int64
		for _, path := range corpusIndex {
			if _, ok := cppParseBlacklist[path]; ok {
				continue
			}

			switch filepath.Ext(path) {
			case ".c", ".h":
				buf := corpus[path]
				chars += int64(len(buf))
				var p *cppParser
				var err error
				if p, err = newCppParser(Source{path, buf}, func(msg string, args ...interface{}) {
					p.close()
					b.Fatalf(msg, args...)
				}); err != nil {
					b.Fatal(path, err)
				}

				ast := p.preprocessingFile()
				if len(ast) == 0 {
					b.Fatalf("%v: empty AST", path)
				}

				eol := ast[len(ast)-1]
				if _, ok := eol.(eofLine); !ok {
					b.Fatalf("%v: AST not terminated: %T", p.pos(), eol)
				}
			}
		}
		b.SetBytes(chars)
	}
}

func TestCPPExpand(t *testing.T) {
	blacklist := map[string]struct{}{
		"010.c":                 {}, //TODO
		"011.c":                 {}, //TODO
		"012.c":                 {}, //TODO
		"013.c":                 {}, //TODO
		"014.c":                 {}, //TODO
		"015.c":                 {}, //TODO
		"example-6.10-8.h":      {}, //TODO
		"example-6.10.2-7.h":    {}, //TODO
		"example-6.10.2-8.h":    {}, //TODO
		"example-6.10.3.3-4.h":  {}, //TODO
		"example-6.10.3.5-3.h":  {}, //TODO
		"example-6.10.3.5-4.h":  {}, //TODO
		"example-6.10.3.5-5.h":  {}, //TODO
		"example-6.10.3.5-6.h":  {}, //TODO
		"example-6.10.3.5-7.h":  {}, //TODO
		"example-6.10.3.5-8.h":  {}, //TODO
		"example-6.10.3.5-9.h":  {}, //TODO
		"example-6.10.3.5-9a.h": {}, //TODO
		"example-6.10.3.5-9b.h": {}, //TODO
		"example-6.10.3.5-9c.h": {}, //TODO
		"example-6.10.3.5-9d.h": {}, //TODO
		"issue131.c":            {}, //TODO

	}
	var fails []string
	var files, ok, skip int
	err := filepath.Walk(filepath.FromSlash("../v3/testdata/cpp-expand/"), func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if info.IsDir() || (!strings.HasSuffix(path, ".c") && !strings.HasSuffix(path, ".h")) {
			return nil
		}

		files++
		switch {
		case re != nil:
			if !re.MatchString(path) {
				skip++
				return nil
			}
		default:
			if _, ok := blacklist[filepath.Base(path)]; ok {
				skip++
				return nil
			}
		}

		if *oTrace {
			fmt.Fprintln(os.Stderr, path)
		}
		var b strings.Builder
		if err := Preprocess(testCfg, []Source{{path, nil}}, &b); err != nil {
			fails = append(fails, path)
			t.Fatalf("%v: %v", path, err)
		}
		if strings.Contains(filepath.ToSlash(path), "/mustfail/") {
			if err != nil {
				return nil
			}

			fails = append(fails, path)
			return fmt.Errorf("%v: unexpected success", path)
		}

		if err != nil {
			fails = append(fails, path)
			return err
		}

		expFn := path + ".expect"
		exp, err := os.ReadFile(expFn)
		if err != nil {
			fails = append(fails, path)
			t.Error(err)
		}

		g := strings.ReplaceAll(b.String(), "\r", "")
		g = strings.TrimSpace(g)
		e := strings.ReplaceAll(string(exp), "\r", "")
		e = strings.TrimSpace(e)
		if g != e {
			fails = append(fails, path)
			diff := difflib.UnifiedDiff{
				A:        difflib.SplitLines(e),
				B:        difflib.SplitLines(g),
				FromFile: expFn,
				ToFile:   path,
				Context:  0,
			}
			s, err := difflib.GetUnifiedDiffString(diff)
			if err != nil {
				t.Fatalf("%v: %v", path, err)
			}

			t.Errorf("%v\ngot\n%s\nexp\n%s\ngot\n%s\nexp\n%s", s, g, e, hex.Dump([]byte(g)), hex.Dump([]byte(e)))
			return nil
		}
		ok++
		return nil
	})
	for _, v := range fails {
		t.Log(v)
	}
	t.Logf("files %v, skip %v, ok %v, fails %v", files, skip, ok, len(fails))
	if err != nil {
		t.Fatal(err)
	}
}
