// Copyright 2017 The CC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build ignore
// +build ignore

package main

import (
	"bytes"
	"flag"
	"fmt"
	"go/format"
	"go/token"
	"io/ioutil"
	"os"
	"path/filepath"
	"runtime"
	"sort"
	"strings"

	"modernc.org/cc/v2"
	"modernc.org/ir"
	"modernc.org/sortutil"
	"modernc.org/strutil"
	"modernc.org/xc"
)

var (
	goOS   = runtime.GOOS
	goArch = runtime.GOARCH
	osArch = fmt.Sprintf("%s_%s", goOS, goArch)

	crt  string
	dict = xc.Dict

	need = map[string][]string{ // "goos_goarch" or "goos" or "all": list of headers
		"all": {
			"alloca.h",
			"assert.h",
			"ctype.h",
			"errno.h",
			"fcntl.h",
			"float.h",
			"inttypes.h",
			"limits.h",
			"locale.h",
			"malloc.h",
			"math.h",
			"setjmp.h",
			"stdarg.h",
			"stdbool.h",
			"stddef.h",
			"stdint.h",
			"stdio.h",
			"stdlib.h",
			"string.h",
			"strings.h",
			"sys/stat.h",
			"sys/time.h",
			"sys/wait.h",
			"time.h",
			"unistd.h",
			"wchar.h",
			"zlib.h",
		},
		"linux": {
			"arpa/inet.h",
			"dirent.h",
			"dlfcn.h",
			"execinfo.h",
			"fts.h",
			"getopt.h",
			"grp.h",
			"memory.h",
			"netdb.h",
			"pthread.h",
			"pwd.h",
			"sched.h",
			"signal.h",
			"signal.h",
			"sys/file.h",
			"sys/ioctl.h",
			"sys/mman.h",
			"sys/param.h",
			"sys/resource.h",
			"sys/select.h",
			"sys/socket.h",
			"sys/statfs.h",
			"sys/times.h",
			"sys/types.h",
			"sys/uio.h",
			"sys/un.h",
			"sys/utsname.h",
			"termios.h",
			"utime.h",
		},
		"windows": {
			"io.h",
			"process.h",
			"windows.h",
		},
	}
)

func main() {
	oCRT := flag.String("crt", "", "CRT import path to update")
	flag.Parse()
	if s := *oCRT; s != "" {
		var err error
		if crt, err = findRepo(s); err != nil {
			panic(err)
		}
	}
	sys := predefined()
	var want []string
	for k, v := range need {
		switch {
		case
			k == "all",
			k == goOS,
			k == osArch:

			want = append(want, v...)
		}
	}
	want = want[:sortutil.Dedupe(sort.StringSlice(want))]
	var got []string
	tweaks := &cc.Tweaks{
		EnableAnonymousStructFields: true,
		EnableEmptyStructs:          true,
		TrackIncludes: func(s string) {
			if strings.HasSuffix(s, "builtin.h") || strings.HasSuffix(s, "predefined.h") {
				return
			}

			got = append(got, s)
		},
	}
	for _, v := range want {
		tu, err := cc.Translate(
			tweaks,
			append([]string{"@"}, sys...),
			sys,
			cc.NewStringSource("main.c", fmt.Sprintf(`
#include "./%s/builtin.h"

// Output of gcc features.c && ./a.out in modernc.org/cc/v2/headers on linux_amd64.
#define _POSIX_SOURCE 1
#define _POSIX_C_SOURCE 200809
#define _DEFAULT_SOURCE 1


#include "%v"
`, osArch, v)),
		)
		if err != nil {
			panic(cc.ErrString(err))
		}

		if crt != "" {
			defs(v, tu)
		}
	}
	got = got[:sortutil.Dedupe(sort.StringSlice(got))]
	for _, v := range got {
		b, err := ioutil.ReadFile(v)
		if err != nil {
			panic(cc.ErrString(err))
		}

		f := filepath.Join(osArch, v)
		mkdir(filepath.Dir(f))
		if err := ioutil.WriteFile(f, b, 0644); err != nil {
			panic(cc.ErrString(err))
		}
	}
}

func predefined() []string {
	predefined, inc, sys, err := cc.HostConfig("-std=c99")
	if err != nil {
		panic(cc.ErrString(err))
	}

	if len(inc) != 0 {
		panic(inc)
	}

	a := strings.Split(predefined, "\n")
	w := 0
	for _, v := range a {
		v = strings.TrimSpace(v)
		if s := strings.ToLower(v); strings.Contains(s, "gcc") || strings.Contains(s, "gnu") {
			continue
		}

		a[w] = v
		w++
	}
	a = a[:w]
	osArch = fmt.Sprintf("%s_%s", goOS, goArch)
	mkdir(osArch)
	b := bytes.NewBufferString(`// Code generated by $ go generate - DO NOT EDIT.

// +build ignore
	
`)
	for _, v := range a {
		fmt.Fprintln(b, v)
	}

	if err := ioutil.WriteFile(filepath.Join(osArch, "predefined.h"), b.Bytes(), 0644); err != nil {
		panic(cc.ErrString(err))
	}

	b.Reset()
	m := map[string]struct{}{}
	w = 0
	for _, v := range sys {
		v := filepath.Clean(v)
		if _, ok := m[v]; !ok {
			fmt.Fprintf(b, "%s\n", v)
			sys[w] = v
			w++
		}
		m[v] = struct{}{}
	}
	sys = sys[:w]
	if err := ioutil.WriteFile(filepath.Join(osArch, "paths"), b.Bytes(), 0664); err != nil {
		panic(cc.ErrString(err))
	}

	return sys
}

func mkdir(p string) {
	if _, err := os.Stat(p); err != nil {
		if !os.IsNotExist(err) {
			panic(cc.ErrString(err))
		}

		if err := os.MkdirAll(p, 0775); err != nil {
			panic(fmt.Errorf("%q: %v", p, err))
		}
	}
}

func findRepo(s string) (string, error) {
	s = filepath.FromSlash(s)
	for _, v := range strings.Split(strutil.Gopath(), string(os.PathListSeparator)) {
		p := filepath.Join(v, "src", s)
		fi, err := os.Lstat(p)
		if err != nil {
			continue
		}

		if fi.IsDir() {
			wd, err := os.Getwd()
			if err != nil {
				return "", err
			}

			if p, err = filepath.Rel(wd, p); err != nil {
				return "", err
			}

			if p, err = filepath.Abs(p); err != nil {
				return "", err
			}

			return p, nil
		}
	}
	return "", fmt.Errorf("%q: cannot find repository", s)
}

func defs(include string, tu *cc.TranslationUnit) {
	model, err := cc.NewModel()
	if err != nil {
		panic(err)
	}

	var a []string
	for _, v := range tu.Macros {
		if v.IsFnLike {
			continue
		}

		from := tu.FileSet.Position(v.DefTok.Pos()).Filename
		if from == "" || strings.Contains(from, "predefined.h") || strings.Contains(from, "builtin.h") {
			continue
		}

		nm := string(dict.S(v.DefTok.Val))
		if strings.HasPrefix(nm, "__") {
			continue
		}

		if strings.HasPrefix(nm, "_") && len(nm) > 1 {
			if c := nm[1]; c >= 'A' && c <= 'Z' {
				continue
			}
		}

		a = append(a, nm)
	}
	sort.Strings(a)
	fn := include[:len(include)-2] // sans .h
	dir := filepath.Join(crt, filepath.FromSlash(fn))
	fn = filepath.Join(dir, fmt.Sprintf("const_%s_%s.go", runtime.GOOS, runtime.GOARCH))
	pn := filepath.Base(dir)
	if token.Lookup(pn).IsKeyword() {
		pn += "_"
	}
	buf := bytes.NewBufferString(fmt.Sprintf(`// Code generated by $ go generate - DO NOT EDIT.

package %s

const (`, pn))
	for _, nm := range a {
		v := tu.Macros[dict.SID(nm)]
		op, err := v.Eval(model, tu.Macros)
		if err != nil {
			continue
		}

		buf.WriteByte('\n')
		switch x := op.Value.(type) {
		case *ir.Float32Value:
			fmt.Fprintf(buf, "X%s = %v", nm, x.Value)
		case *ir.Float64Value:
			fmt.Fprintf(buf, "X%s = %v", nm, x.Value)
		case *ir.Int64Value:
			switch {
			case op.Type.IsUnsigned():
				fmt.Fprintf(buf, "X%s = %v", nm, uint64(cc.ConvertInt64(x.Value, op.Type, model)))
			default:
				fmt.Fprintf(buf, "X%s = %v", nm, x.Value)
			}
		case *ir.StringValue:
			fmt.Fprintf(buf, "X%s = %q", nm, dict.S(int(x.StringID)))
		default:
			panic(fmt.Errorf("%T", x))
		}
	}
	a = a[:0]
	for _, v := range tu.FileScope.Idents {
		switch x := v.(type) {
		case *cc.EnumerationConstant:
			nm := string(dict.S(x.Token.Val))
			if strings.HasPrefix(nm, "__") {
				continue
			}

			a = append(a, string(dict.S(x.Token.Val)))
		}
	}
	sort.Strings(a)
	if len(a) != 0 {
		buf.WriteByte('\n')
	}
	for _, nm := range a {
		op := tu.FileScope.Idents[dict.SID(nm)].(*cc.EnumerationConstant).Operand
		x := op.Value.(*ir.Int64Value)
		buf.WriteByte('\n')
		switch {
		case op.Type.IsUnsigned():
			fmt.Fprintf(buf, "C%s = %v", nm, uint64(cc.ConvertInt64(x.Value, op.Type, model)))
		default:
			fmt.Fprintf(buf, "C%s = %v", nm, x.Value)
		}
	}
	buf.WriteString("\n)\n")
	b, err := format.Source(buf.Bytes())
	if err != nil {
		fmt.Printf("%s\n", buf.Bytes())
		panic(err)
	}
	mkdir(dir)
	if err := ioutil.WriteFile(fn, b, 0644); err != nil {
		panic(err)
	}
}
