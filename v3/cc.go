// Copyright 2019 The CC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//TODO https://todo.sr.ht/~mcf/cc-issues/34
//TODO http://mcpp.sourceforge.net/ "Provides a validation suite to test C/C++ preprocessor's conformance and quality comprehensively."

//go:generate rm -f lexer.go
//go:generate golex -o lexer.go lexer.l

//go:generate rm -f ast.go
//go:generate yy -o /dev/null -position -astImport "\"fmt\"\n\n\"modernc.org/token\"" -prettyString PrettyString -kind Case -noListKind -noPrivateHelpers -forceOptPos parser.yy

//go:generate stringer -output stringer.go -linecomment -type=Kind,Linkage

//go:generate sh -c "go test -run ^Example |fe"

// Package cc is a C99 compiler front end. Work in progress.
//
// Links
//
// Referenced from elsewhere:
//
//  [0]: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf
//  [1]: https://www.spinellis.gr/blog/20060626/cpp.algo.pdf
//  [2]: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf
//  [3]: http://gallium.inria.fr/~fpottier/publis/jourdan-fpottier-2016.pdf
//  [4]: https://gcc.gnu.org/onlinedocs/gcc-8.3.0/gcc/Attribute-Syntax.html#Attribute-Syntax
package cc // import "modernc.org/cc/v3"

import (
	"fmt"
	goscanner "go/scanner"
	gotoken "go/token"
	"math"
	"os"
	"os/exec"
	"reflect"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"sync"

	"modernc.org/strutil"
	"modernc.org/token"
)

const (
	scopeParent StringID = -1
)

var (
	_ Pragma = (*pragma)(nil)

	cache       = newPPCache()
	dict        = newDictionary()
	dictStrings [math.MaxUint8 + 1]string
	noPos       token.Position

	debugIncludePaths bool
	debugWorkingDir   bool
	isTesting         bool

	idPtrdiffT = dict.sid("ptrdiff_t")
	idSizeT    = dict.sid("size_t")
	idWCharT   = dict.sid("wchar_t")

	token4Pool = sync.Pool{New: func() interface{} { r := make([]token4, 0); return &r }} //DONE benchmrk tuned capacity
	tokenPool  = sync.Pool{New: func() interface{} { r := make([]Token, 0); return &r }}  //DONE benchmrk tuned capacity

	printHooks = strutil.PrettyPrintHooks{
		reflect.TypeOf(Token{}): func(f strutil.Formatter, v interface{}, prefix, suffix string) {
			t := v.(Token)
			if (t == Token{}) {
				return
			}

			f.Format(prefix)
			r := t.Rune
			if p := t.Position(); p.IsValid() {
				f.Format("%v: ", p)
			}
			s := tokName(r)
			if x := s[0]; x >= '0' && x <= '9' {
				s = strconv.QuoteRune(r)
			}
			f.Format("%s", s)
			if s := t.Value.String(); len(s) != 0 {
				f.Format(" %q", s)
			}
			f.Format(suffix)
		},
	}
)

type Linkage int

// Pragma defines behavior of the object passed to Config.PragmaHandler.
type Pragma interface {
	Error(msg string, args ...interface{}) // Report error.
	MaxAligment() int                      // Returns the current maximum alignment. May return zero.
	MaxInitialAligment() int               // Support #pragma pack(). Returns the maximum alignment in effect at start. May return zero.
	PopMacro(string)
	PushMacro(string)
	SetAlignment(n int) // Support #pragma pack(n)
}

type pragma struct {
	tok cppToken
	c   *cpp
}

func (p *pragma) Error(msg string, args ...interface{}) { p.c.err(p.tok, msg, args...) }

func (p *pragma) MaxAligment() int { return p.c.ctx.maxAlign }

func (p *pragma) MaxInitialAligment() int { return p.c.ctx.maxAlign0 }

func (p *pragma) SetAlignment(n int) {
	if n <= 0 {
		p.Error("%T.SetAlignment(%d): invalid argument", p, n)
		return
	}

	p.c.ctx.maxAlign = n
}

func (p *pragma) PushMacro(nm string) {
	id := dict.sid(nm)
	if p.c.macroStack == nil {
		p.c.macroStack = map[StringID][]*macro{}
	}
	if m := p.c.macros[id]; m != nil {
		p.c.macroStack[id] = append(p.c.macroStack[id], p.c.macros[id])
	}
}

func (p *pragma) PopMacro(nm string) {
	id := dict.sid(nm)
	a := p.c.macroStack[id]
	if n := len(a); n != 0 {
		p.c.macros[id] = a[n-1]
		p.c.macroStack[id] = a[:n-1]
	}
}

// PrettyString returns a formatted representation of things produced by this package.
func PrettyString(v interface{}) string {
	return strutil.PrettyString(v, "", "", printHooks)
}

// StringID is a process-unique string numeric identifier. Its zero value
// represents an empty string.
type StringID int32

// String implements fmt.Stringer.
func (n StringID) String() (r string) {
	if n < 256 {
		return dictStrings[byte(n)]
	}

	dict.mu.RLock()
	r = dict.strings[n]
	dict.mu.RUnlock()
	return r
}

// Node is implemented by Token and all AST nodes.
type Node interface {
	Position() token.Position
}

type noder struct{}

func (noder) Position() token.Position { panic(internalError()) }

// Scope maps identifiers to definitions.
type Scope map[StringID][]Node

func (s *Scope) new() (r Scope) {
	if *s == nil {
		*s = map[StringID][]Node{}
	}
	r = Scope{scopeParent: []Node{struct {
		noder
		Scope
	}{Scope: *s}}}
	return r
}

func (s *Scope) declare(nm StringID, n Node) {
	if *s == nil {
		*s = map[StringID][]Node{nm: {n}}
		// t := ""
		// if x, ok := n.(*Declarator); ok && x.IsTypedefName {
		// 	t = ", typedefname"
		// }
		// dbg("declared %s%s at %v in scope %p", nm, t, n.Position(), *s)
		return
	}

	(*s)[nm] = append((*s)[nm], n)
	// t := ""
	// if x, ok := n.(*Declarator); ok && x.IsTypedefName {
	// 	t = ", typedefname"
	// }
	// dbg("declared %s%s at %v in scope %p", nm, t, n.Position(), *s)
}

// Parent returns s's outer scope, if any.
func (s Scope) Parent() Scope {
	if s == nil {
		return nil
	}

	if x, ok := s[scopeParent]; ok {
		return x[0].(struct {
			noder
			Scope
		}).Scope
	}

	return nil
}

func (s *Scope) typedef(nm StringID, tok Token) *Declarator {
	if d := s.declarator(nm, tok); d != nil && d.IsTypedefName {
		return d
	}

	return nil
}

func (s *Scope) identifier(nm StringID, tok Token) *Declarator {
	if d := s.declarator(nm, tok); d != nil && !d.IsTypedefName {
		return d
	}

	return nil
}

func (s *Scope) declarator(nm StringID, tok Token) *Declarator {
	seq := tok.seq
	for s := *s; s != nil; s = s.Parent() {
		for _, v := range s[nm] {
			switch x := v.(type) {
			case *Declarator:
				if !x.isVisible(seq) {
					continue
				}

				return x
			case *Enumerator:
				return nil
			case *EnumSpecifier, *StructOrUnionSpecifier, *StructDeclarator:
				// nop
			default:
				panic(internalError())
			}
		}
	}
	return nil
}

func (s *Scope) enumerator(nm StringID, tok Token) *Enumerator {
	seq := tok.seq
	for s := *s; s != nil; s = s.Parent() {
		for _, v := range s[nm] {
			switch x := v.(type) {
			case *Declarator:
				if !x.isVisible(seq) {
					continue
				}

				return nil
			case *Enumerator:
				return x
			case *EnumSpecifier, *StructOrUnionSpecifier, *StructDeclarator:
				// nop
			default:
				panic(internalError())
			}
		}
	}
	return nil
}

// Config3 amends behavior of translation phases 1 to 3.
type Config3 struct {
	WorkingDir string // Overrides os.Getwd if non empty.

	MaxSourceLine int // Zero: Scanner will use default buffer. Non zero: Scanner will use max(default buffer size, MaxSourceLine).

	PreserveWhiteSpace                      bool // Including also comments.
	RejectElseExtraTokens                   bool // Pedantic: do not silently accept "#else foo".
	RejectEndifExtraTokens                  bool // Pedantic: do not silently accept "#endif foo".
	RejectFinalBackslash                    bool // Pedantic: do not silently accept "foo\\\n".
	RejectFunctionMacroEmptyReplacementList bool // Pedantic: do not silently accept "#define foo(bar)\n".
	RejectIfdefExtraTokens                  bool // Pedantic: do not silently accept "#ifdef foo bar".
	RejectIfndefExtraTokens                 bool // Pedantic: do not silently accept "#ifndef foo bar".
	RejectIncludeNext                       bool // Pedantic: do not silently accept "#include_next".
	RejectInvalidVariadicMacros             bool // Pedantic: do not silently accept "#define foo(bar...)". Standard allows only #define foo(bar, ...)
	RejectLineExtraTokens                   bool // Pedantic: do not silently accept "#line 1234 \"foo.c\" bar".
	RejectMissingFinalNewline               bool // Pedantic: do not silently accept "foo\nbar".
	RejectUndefExtraTokens                  bool // Pedantic: do not silently accept "#undef foo bar".
}

// Config amends behavior of translation phase 4 and above. Instances of Config
// are not mutated by this package and it's safe to share/reuse them.
//
// The *Config passed to Parse or Translate should not be mutated afterwards.
type Config struct {
	Config3
	ABI ABI

	PragmaHandler func(Pragma, []Token) // Called on pragmas, other than #pragma STDC ..., if non nil

	MaxErrors int // 0: default (10), < 0: unlimited, n: n.

	DebugIncludePaths                      bool // Output to stderr.
	DebugWorkingDir                        bool // Output to stderr.
	InjectTracingCode                      bool // Output to stderr.
	PreprocessOnly                         bool
	RejectAnonymousFields                  bool // Pedantic: do not silently accept "struct{int;}".
	RejectCaseRange                        bool // Pedantic: do not silently accept "case 'a'...'z':".
	RejectEmptyCompositeLiterals           bool // Pedantic: do not silently accept "foo = (T){}".
	RejectEmptyDeclarations                bool // Pedantic: do not silently accept "int foo(){};".
	RejectEmptyInitializerList             bool // Pedantic: do not silently accept "foo f = {};".
	RejectEmptyStructs                     bool // Pedantic: do not silently accept "struct foo {};".
	RejectLabelValues                      bool // Pedantic: do not silently accept "foo: bar(); void *ptr = &&foo;" or "goto *ptr".
	RejectLateBinding                      bool // Pedantic: do not silently accept void f() { g(); } void g() {}
	RejectMissingConditionalExpr           bool // Pedantic: do not silently accept "foo = bar ? : baz;".
	RejectMissingDeclarationSpecifiers     bool // Pedantic: do not silently accept "main() {}".
	RejectMissingFinalStructFieldSemicolon bool // Pedantic: do not silently accept "struct{int i; int j}".
	RejectNestedFunctionDefinitions        bool // Pedantic: do not silently accept nested function definitons.
	RejectParamSemicolon                   bool // Pedantic: do not silently accept "int f(int a; int b)".
	RejectStatementExpressions             bool // Pedantic: do not silently accept "i = ({foo();})".
	RejectTypeof                           bool // Pedantic: do not silently accept "typeof foo" or "typeof(bar*)".
	RejectUninitializedDeclarators         bool // Reject int f() { int j; return j; }
	doNotSanityCheckComplexTypes           bool // Testing only
	fakeIncludes                           bool // Testing only.
	ignoreErrors                           bool // Testing only.
	ignoreIncludes                         bool // Testing only.
	ignoreUndefinedIdentifiers             bool // Testing only.
}

type context struct {
	breaks      int
	casePromote Type
	cases       []*LabeledStatement // switch
	cfg         *Config
	checkFn     *FunctionDefinition
	closure     map[StringID]struct{}
	continues   int
	goscanner.ErrorList
	includePaths    []string
	intBits         int
	intMaxWidth     int64 // Set if the preprocessor saw __INTMAX_WIDTH__.
	keywords        map[StringID]rune
	maxAlign        int // If non zero: maximum alignment of members of structures (other than zero-width bitfields).
	maxAlign0       int
	maxErrors       int
	mode            mode
	modes           []mode
	mu              sync.Mutex
	ptrdiffT        Type
	readDelta       int
	sizeT           Type
	structs         map[StructInfo]struct{}
	switches        int
	sysIncludePaths []string
	tuSize          int64 // Sum of sizes of processed inputs
	tuSources       int   // Number of processed inputs
	wcharT          Type

	capture bool
}

func newContext(cfg *Config) *context {
	maxErrors := cfg.MaxErrors
	if maxErrors == 0 {
		maxErrors = 10
	}
	return &context{
		cfg:       cfg,
		keywords:  keywords,
		maxErrors: maxErrors,
		structs:   map[StructInfo]struct{}{},
	}
}

func (c *context) stddef(nm StringID, s Scope, tok Token) Type {
	if d := s.typedef(nm, tok); d != nil {
		if t := d.Type(); t != nil && t.Kind() != Invalid {
			return t
		}
	}

	c.errNode(&tok, "undefined: %s", nm)
	return noType
}

func (c *context) errNode(n Node, msg string, args ...interface{}) (stop bool) {
	return c.err(n.Position(), msg, args...)
}

func (c *context) err(pos token.Position, msg string, args ...interface{}) (stop bool) {
	// dbg("FAIL "+msg, args...)
	//fmt.Printf("FAIL "+msg+"\n", args...)
	if c.cfg.ignoreErrors {
		return false
	}

	s := fmt.Sprintf(msg, args...)
	c.mu.Lock()
	max := c.maxErrors
	switch {
	case max < 0 || max > len(c.ErrorList):
		c.ErrorList.Add(gotoken.Position(pos), s)
	default:
		stop = true
	}
	c.mu.Unlock()
	return stop
}

func (c *context) errs(list goscanner.ErrorList) (stop bool) {
	c.mu.Lock()

	defer c.mu.Unlock()

	max := c.maxErrors
	for _, v := range list {
		switch {
		case max < 0 || max > len(c.ErrorList):
			c.ErrorList = append(c.ErrorList, v)
		default:
			return true
		}
	}
	return false
}

func (c *context) Err() error {
	c.mu.Lock()
	switch x := c.ErrorList.Err().(type) {
	case goscanner.ErrorList:
		x = append(goscanner.ErrorList(nil), x...)
		c.mu.Unlock()
		var lpos gotoken.Position
		w := 0
		for _, v := range x {
			if lpos.Filename != "" {
				if v.Pos.Filename == lpos.Filename && v.Pos.Line == lpos.Line {
					continue
				}
			}

			x[w] = v
			w++
			lpos = v.Pos
		}
		x = x[:w]
		sort.Slice(x, func(i, j int) bool {
			a := x[i]
			b := x[j]
			if !a.Pos.IsValid() && b.Pos.IsValid() {
				return true
			}

			if a.Pos.IsValid() && !b.Pos.IsValid() {
				return false
			}

			if a.Pos.Filename < b.Pos.Filename {
				return true
			}

			if a.Pos.Filename > b.Pos.Filename {
				return false
			}

			if a.Pos.Line < b.Pos.Line {
				return true
			}

			if a.Pos.Line > b.Pos.Line {
				return false
			}

			return a.Pos.Column < b.Pos.Column
		})
		a := make([]string, 0, len(x))
		for _, v := range x {
			a = append(a, v.Error())
		}
		return fmt.Errorf("%s", strings.Join(a, "\n"))
	default:
		c.mu.Unlock()
		return x
	}
}

func (c *context) not(n Node, mode mode) {
	if c.mode&mode != 0 {
		switch mode {
		case mIntConstExpr:
			c.errNode(n, "invalid integer constant expression")
		default:
			panic(internalError())
		}
	}
}

func (c *context) push(mode mode) {
	c.modes = append(c.modes, c.mode)
	c.mode = mode
}

func (c *context) pop() {
	n := len(c.modes)
	c.mode = c.modes[n-1]
	c.modes = c.modes[:n-1]
}

// HostConfig returns the system C preprocessor/compiler configuration, or an
// error, if any.  The configuration is obtained by running the command named
// by the cpp argumnent or "cpp" when it's empty.  For the predefined macros
// list the '-dM' options is added. For the include paths lists, the option
// '-v' is added and the output is parsed to extract the "..." include and
// <...> include paths. To add any other options to cpp, list them in opts.
//
// The function relies on a POSIX/GCC compatible C preprocessor installed.
// Execution of HostConfig is not free, so caching of the results is
// recommended.
func HostConfig(cpp string, opts ...string) (predefined string, includePaths, sysIncludePaths []string, err error) {
	if cpp == "" {
		cpp = "cpp"
	}
	args := append(append([]string{"-dM"}, opts...), os.DevNull)
	pre, err := exec.Command(cpp, args...).Output()
	if err != nil {
		return "", nil, nil, err
	}

	args = append(append([]string{"-v"}, opts...), os.DevNull)
	out, err := exec.Command(cpp, args...).CombinedOutput()
	if err != nil {
		return "", nil, nil, err
	}

	sep := "\n"
	if env("GOOS", runtime.GOOS) == "windows" {
		sep = "\r\n"
	}

	a := strings.Split(string(out), sep)
	for i := 0; i < len(a); {
		switch a[i] {
		case "#include \"...\" search starts here:":
		loop:
			for i = i + 1; i < len(a); {
				switch v := a[i]; {
				case strings.HasPrefix(v, "#") || v == "End of search list.":
					break loop
				default:
					includePaths = append(includePaths, strings.TrimSpace(v))
					i++
				}
			}
		case "#include <...> search starts here:":
			for i = i + 1; i < len(a); {
				switch v := a[i]; {
				case strings.HasPrefix(v, "#") || v == "End of search list.":
					return string(pre), includePaths, sysIncludePaths, nil
				default:
					sysIncludePaths = append(sysIncludePaths, strings.TrimSpace(v))
					i++
				}
			}
		default:
			i++
		}
	}
	return "", nil, nil, fmt.Errorf("failed parsing %s -v output", cpp)
}

func env(key, val string) string {
	if s := os.Getenv(key); s != "" {
		return s
	}

	return val
}

// Token is a grammar terminal.
type Token struct {
	Rune   rune     // ';' or IDENTIFIER etc.
	Sep    StringID // If Config3.PreserveWhiteSpace is in effect: All preceding white space, if any, combined, including comments.
	Value  StringID // ";" or "foo" etc.
	fileID int32
	pos    int32
	seq    int32
}

// Seq returns t's sequential number.
//
// Comparing positions as in 'before', 'after' is complicated as tokens in a
// translation unit usually come from more than one source file. Macro
// expansion further complicates that. The solution is to sequentially
// numbering the tokens as they are finally seen by the parser, so the usual
// artithmetic '<', '>' operators can be used for that purpose.
func (t Token) Seq() int { return int(t.seq) }

// String implements fmt.Stringer.
func (t Token) String() string { return t.Value.String() }

// Position implements Node.
func (t *Token) Position() (r token.Position) {
	if t.pos != 0 && t.fileID > 0 {
		if file := cache.file(t.fileID); file != nil {
			r = file.PositionFor(token.Pos(t.pos), true)
		}
	}
	return r
}

func tokStr(toks interface{}, sep string) string {
	var b strings.Builder
	switch x := toks.(type) {
	case []token3:
		for i, v := range x {
			if i != 0 {
				b.WriteString(sep)
			}
			b.WriteString(v.String())
		}
	case []token4:
		for i, v := range x {
			if i != 0 {
				b.WriteString(sep)
			}
			b.WriteString(v.String())
		}
	case []cppToken:
		for i, v := range x {
			if i != 0 {
				b.WriteString(sep)
			}
			b.WriteString(v.String())
		}
	case []Token:
		for i, v := range x {
			if i != 0 {
				b.WriteString(sep)
			}
			b.WriteString(v.String())
		}
	default:
		panic(internalError())
	}
	return b.String()
}

func internalError() int                               { return internalErrorf("") }
func internalErrorf(s string, args ...interface{}) int { panic(fmt.Errorf(s, args...)) }
