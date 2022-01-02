// Copyright 2021 The CC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package cc // import "modernc.org/cc/v4"

import (
	"fmt"
	"strings"
)

var (
	_ tokenSequence = (*cat)(nil)
	_ tokenSequence = (*preprocessingTokens)(nil)
	_ tokenSequence = (*tokenizer)(nil)
	_ tokenSequence = (*tokens)(nil)

	comma = []byte{','}
	sp    = []byte{' '}
)

type tokens []Token

func (p tokens) peek(index int) (tok Token) {
	if index < len(p) {
		return p[index]
	}

	tok.Ch = eof
	return tok
}

func (p *tokens) read() (tok Token, hs hideSet, ok bool) {
	s := *p
	if len(s) == 0 {
		return tok, nil, false
	}

	tok = s[0]
	if tok.Ch != eof {
		s = s[1:]
		*p = s
	}
	return tok, nil, true
}

// Macro represents a preprocessor #define.
type Macro struct {
	Name            Token
	Params          []Token
	ReplacementList []Token
	params          formalParams

	MinArgs int // m x: 0, m() x: 0, m(...): 0, m(a) a: 1, m(a, ...): 1, m(a, b): 2, m(a, b, ...): 2.
	VarArg  int // m(a): -1, m(...): 0, m(a, ...): 1, m(a...): 0, m(a, b...): 1.

	IsFnLike bool // m: false, m(): true, m(x): true.
}

func newMacro(nm Token, params, replList []Token, minArgs, varArg int, isFnLike bool) (*Macro, error) {
	var fp formalParams
	for _, v := range params {
		fp = append(fp, string(v.Src()))
	}
	if len(fp) > 1 {
		m := map[string]struct{}{}
		for i, v := range fp {
			if _, ok := m[v]; ok {
				return nil, errorf("%v: duplicate parameter name", params[i].Position())
			}
		}
	}
	return &Macro{
		IsFnLike:        isFnLike,
		MinArgs:         minArgs,
		Name:            nm,
		Params:          params,
		ReplacementList: replList,
		VarArg:          varArg,
		params:          fp,
	}, nil
}

func (m *Macro) ts(t Token) tokenSequence {
	r := m.ReplacementList
	if len(t.Sep()) != 0 {
		t.Ch = ' '
		t.Set(nil, sp)
		r = append([]Token{t}, r...)
	}
	return (*tokens)(&r)
}

func (m *Macro) fp() formalParams {
	if !m.IsFnLike {
		return nil
	}

	return m.params
}

type hideSet map[string]struct{}

func (h *hideSet) String() string {
	if h == nil {
		return ""
	}

	hs := *h
	var a []string
	for k := range hs {
		a = append(a, k)
	}
	return fmt.Sprintf("%s", a)
}

func (h hideSet) add(s string) hideSet {
	r := hideSet{}
	for k := range h {
		r[k] = struct{}{}
	}
	r[s] = struct{}{}
	return r
}

func (h hideSet) has(s string) bool {
	if h == nil {
		return false
	}

	for k := range h {
		if s == k {
			return true
		}
	}

	return false
}

// ∪ - union.
func (h hideSet) cup(src hideSet) hideSet {
	if h == nil {
		return src
	}

	if src == nil {
		return h
	}

	r := hideSet{}
	for k := range h {
		r[k] = struct{}{}
	}
	for k := range src {
		r[k] = struct{}{}
	}
	return r
}

// ∩ - intersection.
func (h hideSet) cap(src hideSet) hideSet {
	if h == nil {
		return nil
	}

	if src == nil {
		return nil
	}

	r := hideSet{}
	for k := range h {
		if _, ok := src[k]; ok {
			r[k] = struct{}{}
		}
	}
	return r
}

type preprocessingToken struct {
	Token
	hs hideSet
}

type tokenizer struct {
	c    *cpp
	line textLine
	toks []preprocessingToken
}

func newTokenizer(c *cpp) *tokenizer {
	return &tokenizer{c: c}
}

func (t *tokenizer) peek(index int) (tok Token) {
	for {
		n := len(t.line)
		if index < n {
			return t.line[index]
		}

		if n != 0 && t.line[n-1].Ch == eof {
			return t.line[n-1]
		}

		t.line = append(t.line, t.c.nextLine()...)
	}
}

func (t *tokenizer) read() (tok Token, hs hideSet, ok bool) {
	if len(t.line) == 0 {
		t.line = t.c.nextLine()
	}

	tok = t.line[0]
	if tok.Ch != eof {
		t.line = t.line[1:]
	}
	return tok, nil, true // The hide set starts empty
}

func (t *tokenizer) token() (tok Token) {
	if len(t.toks) == 0 {
		t.toks = t.c.expand(t)
	}
	tok = t.toks[0].Token
	if tok.Ch != eof {
		t.toks = t.toks[1:]
	}
	return tok
}

type tokenSequence interface {
	peek(int) Token
	read() (tok Token, hs hideSet, ok bool)
}

type cat struct {
	head preprocessingTokens
	tail tokenSequence
}

func newCat(head preprocessingTokens, tail tokenSequence) tokenSequence {
	if len(head) == 0 {
		return tail
	}

	switch x := tail.(type) {
	case *tokenizer:
		return &cat{head, tail}
	case *cat:
		if len(x.head) == 0 {
			return &cat{head, x.tail}
		}

		return &cat{head, tail}
	case *preprocessingTokens:
		t := *x
		if len(t) == 0 {
			return &head
		}

		head = append(head[:len(head):len(head)], t...)
		return &head
	case *tokens:
		if len(*x) == 0 {
			return &head
		}

		return &cat{head, x}
	default:
		panic(todo("%T", x))
	}
}

func (c *cat) peek(index int) (tok Token) {
	if index < len(c.head) {
		return c.head.peek(index)
	}

	return c.tail.peek(index - len(c.head))
}

func (c *cat) read() (tok Token, hs hideSet, ok bool) {
	if len(c.head) != 0 {
		return c.head.read()
	}

	return c.tail.read()
}

type preprocessingTokens []preprocessingToken

func (p preprocessingTokens) peek(index int) (tok Token) {
	if index < len(p) {
		return p[index].Token
	}

	tok.Ch = eof
	return tok
}

func (p *preprocessingTokens) read() (tok Token, hs hideSet, ok bool) {
	s := *p
	if len(s) == 0 {
		return tok, nil, false
	}

	tok = s[0].Token
	hs = s[0].hs
	if tok.Ch != eof {
		s = s[1:]
		*p = s
	}
	return tok, hs, true
}

type formalParams []string

func (fp formalParams) is(s string) int {
	for i, v := range fp {
		if s == v {
			return i
		}
	}
	return -1
}

// cpp is the C preprocessor.
type cpp struct {
	eh          errHandler
	groups      map[string]group
	indentLevel int // debug dumps
	macros      map[string]*Macro
	sources     []Source
	stack       []interface{}
	tok         Token
	tokenizer   *tokenizer
	tos         interface{}

	closed bool
}

// newCPP returns a newly created cpp.
func newCPP(sources []Source, eh errHandler) (*cpp, error) {
	m := map[string]struct{}{}
	for _, v := range sources {
		if _, ok := m[v.Name]; ok {
			return nil, errorf("duplicate source name: %q", v.Name)
		}

		m[v.Name] = struct{}{}
	}
	c := &cpp{
		groups:  map[string]group{},
		eh:      eh,
		macros:  map[string]*Macro{},
		sources: sources,
	}
	c.tokenizer = newTokenizer(c)
	c.tok.Ch = eof // Invalidate
	return c, nil
}

// consume returns c.tok and invalidates c.tok.Ch.
func (c *cpp) consume() (r Token) {
	r = c.tok
	c.tok.Ch = eof
	return r
}

// nextLine returns the next input textLine.
func (c *cpp) nextLine() textLine {
	for {
		// a := []string{fmt.Sprintf("%T", c.tos)}
		// for _, v := range c.stack {
		// 	a = append(a, fmt.Sprintf("%T", v))
		// }
		// trc("STACK %v", a)
		switch x := c.tos.(type) {
		case nil:
			// trc("<nil>")
			if len(c.sources) == 0 {
				return nil
			}

			src := c.sources[0]
			c.sources = c.sources[1:]
			c.push(c.group(src))
		case group:
			// trc("group len %v", len(x))
			if len(x) == 0 {
				c.pop()
				break
			}

			c.tos = x[1:]
			c.push(x[0])
		case controlLine:
			// trc("controlLine %v", toksDump([]Token(x)))
			c.pop()
			switch string(x[1].Src()) {
			case "define":
				c.define(x)
			case "undef":
				c.undef(x)
			default:
				panic(todo("%v: %q", x[0].Position(), x[1].Src()))
			}
		case textLine:
			// trc("textLine %v", toksDump([]Token(x)))
			c.pop()
			return x
		case eofLine:
			// trc("eofLine")
			// Keep it on stack, EOF is sticky
			return textLine([]Token{Token(x)})
		case *ifSection:
			// trc("ifSection")
			switch {
			case x.ifGroup != nil:
				switch {
				case c.ifGroup(x.ifGroup):
					c.pop()
					c.push(x.ifGroup.group)
				default:
					panic(todo(""))
				}
			default:
				panic(todo(""))
			}
		default:
			panic(todo("internal error: %T", x))
		}
	}
}

func (c *cpp) ifGroup(ig *ifGroup) bool {
	ln := ig.line[:len(ig.line)-1] // Remove new-line
	switch string(ln[1].Src()) {
	case "ifndef":
		if len(ln) < 3 { // '#' "ifndef" IDENTIFIER
			c.eh("%v: expected identifier", ln[1].Position())
			return false
		}

		_, ok := c.macros[string(ln[2].Src())]
		return !ok
	case "if":
		if len(ln) < 3 { // '#' "if" <expr>
			c.eh("%v: expected expression", ln[1].Position())
			return false
		}

		return c.isNonZero(c.eval(tokens(ln[2:])))
	default:
		panic(todo("", toksDump(ln)))
	}
}

func (c *cpp) eval(s tokens) interface{} {
	trc("", toksDump(s))
	s2 := c.expand(&s)
	s = s[:0]
	for _, v := range s2 {
		if v.Ch != ' ' {
			s = append(s, v.Token)
		}
	}
	panic(todo("", toksDump(s)))
}

func (c *cpp) isNonZero(val interface{}) bool {
	switch x := val.(type) {
	case nil:
		return false
	case int64:
		return x != 0
	case uint64:
		return x != 0
	default:
		c.eh("", errorf("internal error: %T", x))
		return false
	}
}

// undef executes an #undef control-line, [0]6.10.
func (c *cpp) undef(ln controlLine) {
	// eg. ["#" "undef" "  x" "      \n"]
	if len(ln) < 3 {
		return
	}

	delete(c.macros, string(ln[2].Src()))
}

// define executes a #define control-line, [0]6.10.
func (c *cpp) define(ln controlLine) {
	def := ln[1]
	ln = ln[2:] // Remove '#' and "define"
	if len(ln) == 0 {
		c.eh("%v: missing macro name", def.Position())
		return
	}
	nm := ln[0]
	ln = ln[1:]
	switch {
	case ln[0].Ch == '(' && len(ln[0].Sep()) == 0:
		// lparen: a ( character not immediately preceded by white-space
		// # define identifier ( args_opt ) replacement-list new-line
		//                     ^ln[0]
		c.defineFnMacro(nm, ln[:len(ln)-1]) // strip new-line
	default:
		// # define identifier replacement-list new-line
		//                     ^ln[0]
		c.defineObjectMacro(nm, ln[:len(ln)-1]) // strip new-line
	}
}

func (c *cpp) defineFnMacro(nm Token, ln []Token) {
	fp, rl, minArgs, varArg := c.parseMacroParams(ln)
	rl = c.parseReplacementList(rl)
	switch {
	case c.macros[string(nm.Src())] != nil:
		panic(todo("", nm.Position(), &nm))
	}
	c.macros[string(nm.Src())] = c.newMacro(nm, fp, rl, minArgs, varArg, true)
}

func (c *cpp) parseMacroParams(ln []Token) (fp, rl []Token, minArgs, vaArg int) {
	if len(ln) == 0 || ln[0].Ch != '(' {
		c.eh("internal error")
		return nil, nil, -1, -1
	}

	lpar := ln[0]
	ln = ln[1:] // remove '('
	vaArg = -1
	for {
		if len(ln) == 0 {
			// (A)
			c.eh("%v: macro paramater list is missing final ')'", lpar.Position())
			return nil, nil, -1, -1
		}

		switch ln[0].Ch {
		case ')':
			// (B)
			ln = ln[1:]
			return fp, ln, minArgs, vaArg
		case rune(IDENTIFIER):
			fp = append(fp, ln[0])
			minArgs++
			ln = ln[1:]
			if len(ln) == 0 {
				break // -> (A)
			}

			switch ln[0].Ch {
			case ')':
				// ok -> (B)
			case ',':
				ln = ln[1:]
			case rune(DDD):
				if vaArg >= 0 {
					c.eh("%v: multiple var arguments", ln[0].Position())
					return nil, nil, -1, -1
				}

				vaArg = len(fp) - 1
				ln = ln[1:]
			default:
				c.eh("%v: unexpected %v", ln[0].Position(), runeName(ln[0].Ch))
				return nil, nil, -1, -1
			}
		case rune(DDD):
			if vaArg >= 0 {
				c.eh("%v: multiple var arguments", ln[0].Position())
				return nil, nil, -1, -1
			}

			vaArg = len(fp)
			ln = ln[1:]
			if len(ln) == 0 {
				break // -> (A)
			}

			switch ln[0].Ch {
			case ')':
				// ok -> (B)
			default:
				ln = nil // -> (A)
			}
		default:
			c.eh("%v: unexpected %v", ln[0].Position(), runeName(ln[0].Ch))
			return nil, nil, -1, -1
		}
	}
}

func (c *cpp) newMacro(nm Token, params, replList []Token, minArgs, varArg int, isFnLike bool) *Macro {
	// trc("nm %q, params %v, replList %v, minArgs %v, varArg %v, isFnLike %v", nm.Src(), toksDump(params), toksDump(replList), minArgs, varArg, isFnLike)
	if string(nm.Src()) == "defined" {
		c.eh("%v: \"defined\" cannot be used as a macro name", nm.Position) // gcc says so.
		return nil
	}

	m, err := newMacro(nm, params, replList, minArgs, varArg, isFnLike)
	if err != nil {
		c.eh("", err)
	}
	return m
}

func (c *cpp) defineObjectMacro(nm Token, ln []Token) {
	rl := c.parseReplacementList(ln)
	switch {
	case c.macros[string(nm.Src())] != nil:
		panic(todo("", nm.Position(), &nm))
	default:
		c.macros[string(nm.Src())] = c.newMacro(nm, nil, rl, -1, -1, false)
	}
}

// parseReplacementList transforms s into preprocessing tokens that have separate
// tokens for white space.
func (c *cpp) parseReplacementList(s []Token) (r []Token) {
	s = c.toksTrim(s)
	for i, v := range s {
		if b := v.Sep(); len(b) != 0 {
			switch {
			case i == 0:
				v.Set(nil, v.Src())
			default:
				w := v
				w.Ch = ' '
				w.len = uint32(len(b))
				w.src = w.sep
				w.sep = w.src
				r = append(r, w)
			}
		}
		r = append(r, v)
	}
	w := 0
	for i, v := range r {
		switch v.Ch {
		case ' ':
			if i != 0 {
				switch r[i-1].Ch {
				case ' ', '#', rune(PPPASTE):
					continue
				}
			}
			if i+1 < len(r) {
				switch r[i+1].Ch {
				case rune(PPPASTE):
					continue
				}
			}
		}

		r[w] = v
		w++
	}
	return r[:w]
}

func (c *cpp) toksTrim(s []Token) []Token {
	for len(s) != 0 && s[0].Ch == ' ' {
		s = s[1:]
	}
	for len(s) != 0 && s[len(s)-1].Ch == ' ' {
		s = s[:len(s)-1]
	}
	return s
}

func (c *cpp) pop() {
	if n := len(c.stack); n != 0 {
		c.tos = c.stack[n-1]
		c.stack = c.stack[:n-1]
		return
	}

	c.tos = nil
}

func (c *cpp) group(src Source) group {
	if g, ok := c.groups[src.Name]; ok {
		return g
	}

	p, err := newCppParser(src, c.eh)
	if err != nil {
		c.eh("", err)
		return nil
	}

	g := p.group(false)
	c.groups[src.Name] = g
	return g
}

func (c *cpp) push(v interface{}) {
	if c.tos != nil {
		c.stack = append(c.stack, c.tos)
	}
	c.tos = v
}

func (c *cpp) indent() string {
	c.indentLevel++
	return fmt.Sprintf("\t%s", strings.Repeat("· ", c.indentLevel-1))
}

func (c *cpp) undent() string {
	c.indentLevel--
	return fmt.Sprintf("\t%s", strings.Repeat("· ", c.indentLevel))
}

// [1], pg 1.
func (c *cpp) expand(TS tokenSequence) (r preprocessingTokens) {
	// trc("  %s%v (%v)", c.indent(), toksDump(TS), origin(2))
	// defer func() { trc("->%s%v", c.undent(), toksDump(r)) }()
	var ptok preprocessingToken
	var ok bool
	ptok.Token, ptok.hs, ok = TS.read()
	if !ok {
		// if TS is {} then
		//	return {};
		return nil
	}

	// trc("%s^%s, ok %v", &ptok.Token, &ptok.hs, ok)
	T := ptok.Token
	HS := ptok.hs
	if T.Ch == eof {
		return preprocessingTokens{ptok}
	}

	src := string(T.Src())
	if HS.has(src) {
		// if TS is T^HS • TS’ and T is in HS then
		//	return T^HS • expand(TS’);
		return append(preprocessingTokens{ptok}, c.expand(TS)...)
	}

	if m := c.macros[src]; m != nil {
	out:
		switch {
		default:
			// if TS is T^HS • TS’ and T is a "()-less macro" then
			//	return expand(subst(ts(T),{},{},HS∪{T},{}) • TS’);
			// trc("  %s<%s is a ()-less macro, expanding to %s>", c.indent(), T.Src(), toksDump(m.ts(T)))
			// defer func() { trc("  %s<%s expanded>", c.undent(), T.Src()) }()
			seq := c.subst(m, m.ts(T), nil, nil, HS.add(src), nil)
			return c.expand(newCat(seq, TS))
		case m.IsFnLike:
			if TS.peek(0).Ch == '(' {
				// if TS is T^HS • ( • TS’ and T is a "()’d macro" then
				//	check TS’ is actuals • )^HS’ • TS’’ and actuals are "correct for T"
				//	return expand(subst(ts(T),fp(T),actuals,(HS∩HS’)∪{T},{}) • TS’’);
				// trc("  %s<%s is a ()'d macro, expanding to %s>", c.indent(), T.Src(), toksDump(m.ts(T)))
				// defer func() { trc("  %s<%s expanded>", c.undent(), T.Src()) }()
				args, rparen, ok := c.parseMacroArgs(TS)
				if !ok {
					return nil
				}

				switch {
				case len(args) < m.MinArgs:
					c.eh("%v: not enough macro arguments", rparen.Position())
					break out
				case len(args) > m.MinArgs && m.VarArg < 0:
					c.eh("%v: too many macro arguments", rparen.Position())
					break out
				}

				return c.expand(newCat(c.subst(m, m.ts(T), m.fp(), args, HS.cap(rparen.hs).add(src), nil), TS))
			}
		}
	}

	// note TS must be T HS • TS’
	// return T HS • expand(TS’);
	return append(preprocessingTokens{ptok}, c.expand(TS)...)
}

// [1], pg 2.
func (c *cpp) subst(m *Macro, IS tokenSequence, FP formalParams, AP []preprocessingTokens, HS hideSet, OS preprocessingTokens) (r preprocessingTokens) {
	// trc("  %s%v, HS %v, FP %v, AP %v, OS %v (%v)", c.indent(), toksDump(IS), &HS, FP, toksDump(AP), toksDump(OS), origin(2))
	// defer func() { trc("->%s%v", c.undent(), toksDump(r)) }()
	var ptok preprocessingToken
	var ok bool
	ptok.Token, ptok.hs, ok = IS.read()
	if !ok {
		// if IS is {} then
		//	return hsadd(HS,OS);
		return c.hsAdd(HS, OS)
	}

	if ptok.Ch == '#' {
		switch tok, skip := c.peekNonBlank(IS); {
		case tok.Ch == rune(IDENTIFIER):
			if i := FP.is(string(tok.Src())); i >= 0 {
				// if IS is # • T • IS’ and T is FP[i] then
				//	return subst(IS’,FP,AP,HS,OS • stringize(select(i,AP )));
				c.skip(IS, skip+1)
				return c.subst(m, IS, FP, AP, HS, append(OS, c.stringize(c.apSelect(m, ptok, AP, i))))
			}
		}
	}

	if ptok.Ch == rune(PPPASTE) {
		switch tok, skip := c.peekNonBlank(IS); {
		case tok.Ch == rune(IDENTIFIER):
			if i := FP.is(string(tok.Src())); i >= 0 {
				// if IS is ## • T • IS’ and T is FP[i] then
				if len(AP[i]) == 0 {
					//	if select(i,AP ) is {} then /* only if actuals can be empty */
					//		return subst(IS’,FP,AP,HS,OS );
					panic(todo("", toksDump(IS), skip, i))
				} else {
					//	else
					//		return subst(IS’,FP,AP,HS,glue(OS,select(i,AP )));
					c.skip(IS, skip+1)
					return c.subst(m, IS, FP, AP, HS, c.glue(OS, c.apSelect(m, ptok, AP, i)))
				}
			}
		}
		// else if IS is ## • T HS’ • IS’ then
		//	return subst(IS’,FP,AP,HS,glue(OS,T^HS’));
		if IS.peek(0).Ch != eof {
			var t2 preprocessingToken
			t2.Token, t2.hs, _ = IS.read()
			return c.subst(m, IS, FP, AP, HS, c.glue(OS, preprocessingTokens{t2}))
		}
	}

	if ppasteTok, skip := c.peekNonBlank(IS); ppasteTok.Ch == rune(PPPASTE) {
		if ptok.Ch == rune(IDENTIFIER) {
			if i := FP.is(string(ptok.Src())); i >= 0 {
				// if IS is T • ##^HS’ • IS’ and T is FP[i] then
				//	if select(i,AP ) is {} then /* only if actuals can be empty */
				if len(AP[i]) == 0 {
					//	{
					//		if IS’ is T’ • IS’’ and T’ is FP[j] then
					//			return subst(IS’’,FP,AP,HS,OS • select(j,AP));
					//		else
					//			return subst(IS’,FP,AP,HS,OS);
					//	}
					trc("", &ppasteTok, skip)
					panic(todo("", toksDump(IS)))
				} else {
					//	else
					//		return subst(##^HS’ • IS’,FP,AP,HS,OS • select(i,AP));
					c.skip(IS, skip)
					return c.subst(m, IS, FP, AP, HS, append(OS, c.apSelect(m, ptok, AP, i)...))
				}
			}
		}
	}

	if len(FP) != 0 {
		if i := FP.is(string(ptok.Src())); i >= 0 {
			// if IS is T • IS’ and T is FP[i] then
			//	return subst(IS’,FP,AP,HS,OS • expand(select(i,AP )));
			sel := c.apSelect(m, ptok, AP, i)
			return c.subst(m, IS, FP, AP, HS, append(OS, c.expand(&sel)...))
		}
	}

	if va := m.VarArg; ptok.Ch == rune(IDENTIFIER) && va >= 0 && string(ptok.Src()) == "__VA_ARGS__" && va <= len(AP) {
		return c.subst(m, IS, FP, AP, HS, append(OS, c.expand(c.varArgs(ptok, AP[va:]))...))
	}

	// note IS must be T HS’ • IS’
	// return subst(IS’,FP,AP,HS,OS • T HS’ );
	return c.subst(m, IS, FP, AP, HS, append(OS, ptok))
}

func (c *cpp) apSelect(m *Macro, t preprocessingToken, AP []preprocessingTokens, i int) preprocessingTokens {
	if m.VarArg < 0 || m.VarArg != i {
		return AP[i]
	}

	return *c.varArgs(t, AP[i:])
}

func (c *cpp) varArgs(t preprocessingToken, AP []preprocessingTokens) *preprocessingTokens {
	var commaSP preprocessingTokens
	if len(AP) > 1 {
		t.Set(nil, comma)
		t.Ch = ','
		t2 := t
		t2.Set(nil, sp)
		t2.Ch = ' '
		commaSP = preprocessingTokens{t, t2}
	}
	var a preprocessingTokens
	for i, v := range AP {
		if i != 0 {
			a = append(a, commaSP...)
		}
		a = append(a, v...)
	}
	return &a
}

func (c *cpp) glue(LS, RS preprocessingTokens) (r preprocessingTokens) {
	// trc("  %sLS %v, RS %v (%v)", c.indent(), toksDump(LS), toksDump(RS), origin(2))
	// defer func() { trc("->%s%v", c.undent(), toksDump(r)) }()
	// if LS is L^HS and RS is R^HS’ • RS’ then
	//	return L&R^(HS∩HS’) • RS’; /* undefined if L&R is invalid */
	if len(LS) == 1 {
		tok := LS[0]
		tok.Set(nil, append(tok.Src(), RS[0].Src()...))
		tok.hs = tok.hs.cap(RS[0].hs)
		return append(preprocessingTokens{tok}, RS[1:]...)
	}

	// note LS must be L^HS • LS’
	// return L^HS • glue(LS’,RS );
	return append(LS[:1:1], c.glue(LS[1:], RS)...)
}

// Given a token sequence, stringize returns a single string literal token
// containing the concatenated spellings of the tokens.
//
// [1] pg. 3
func (c *cpp) stringize(s0 preprocessingTokens) (r preprocessingToken) {
	// trc("  %s%v (%v)", c.indent(), toksDump(s0), origin(2))
	// defer func() { trc("->%s%v: %s", c.undent(), toksDump(preprocessingTokens{r}), r.Src()) }()
	if len(s0) == 0 {
		c.eh("", errorf("internal error"))
	}

	r = s0[0]

	// [0]6.10.3.2
	//
	// Each occurrence of white space between the argument’s preprocessing
	// tokens becomes a single space character in the character string
	// literal.
	s := make([]Token, 0, len(s0))
	var last rune
	for _, v := range s0 {
		if v.Ch == ' ' {
			if last == ' ' {
				continue
			}
		}

		last = v.Ch
		s = append(s, v.Token)
	}

	// White space before the first preprocessing token and after the last
	// preprocessing token composing the argument is deleted.
	s = c.toksTrim(s)

	// The character string literal corresponding to an empty argument is
	// ""
	if len(s) == 0 {
		r.Ch = rune(STRINGLITERAL)
		r.Set(nil, []byte(`""`))
		return r
	}

	var a []string
	// Otherwise, the original spelling of each preprocessing token in the
	// argument is retained in the character string literal, except for
	// special handling for producing the spelling of string literals and
	// character constants: a \ character is inserted before each " and \
	// character of a character constant or string literal (including the
	// delimiting " characters), except that it is implementation-defined
	// whether a \ character is inserted before the \ character beginning a
	// universal character name.
	for _, v := range s {
		s := string(v.Src())
		switch v.Ch {
		case rune(CHARCONST), rune(STRINGLITERAL), rune(LONGCHARCONST), rune(LONGSTRINGLITERAL):
			s = strings.ReplaceAll(s, `\`, `\\`)
			s = strings.ReplaceAll(s, `"`, `\"`)
		}
		a = append(a, s)
	}
	r.Ch = rune(STRINGLITERAL)
	r.Set(nil, []byte(`"`+strings.Join(a, "")+`"`))
	return r
}

func (c *cpp) skip(s tokenSequence, n int) {
	for ; n != 0; n-- {
		s.read()
	}
}

func (c *cpp) peekNonBlank(s tokenSequence) (tok Token, skip int) {
	for {
		tok = s.peek(skip)
		if tok.Ch != ' ' {
			return tok, skip
		}

		skip++
	}
}

func (c *cpp) parseMacroArgs(TS tokenSequence) (args []preprocessingTokens, rparen preprocessingToken, ok bool) {
	// trc("  %sin  %v (%v)", c.indent(), toksDump(TS), origin(2))
	// defer func() { trc("->%sout %v", c.undent(), toksDump(args)) }()
	var tok preprocessingToken
	if tok.Token, tok.hs, ok = TS.read(); !ok || tok.Ch != '(' {
		c.eh("%v: expected macro argument list starting with '('", tok.Position())
		return nil, preprocessingToken{}, false
	}

	var arg preprocessingTokens
	level := 0
out:
	for {
		switch t := TS.peek(0); t.Ch {
		case ',':
			if level != 0 {
				tok.Token, tok.hs, _ = TS.read()
				arg = append(arg, tok)
				break
			}

			args = append(args, arg)
			TS.read() // discard the ','
			arg = nil
		case '(':
			tok.Token, tok.hs, _ = TS.read()
			arg = append(arg, tok)
			level++
		case ')':
			if level == 0 {
				args = append(args, arg)
				break out
			}

			tok.Token, tok.hs, _ = TS.read()
			arg = append(arg, tok)
			level--
		case eof:
			panic(todo("", &t))
		default:
			tok.Token, tok.hs, _ = TS.read()
			arg = append(arg, tok)
		}
	}

	if tok.Token, tok.hs, ok = TS.read(); tok.Ch != ')' {
		c.eh("%v: expected macro argument list terminating ')'", tok.Position())
		return nil, preprocessingToken{}, false
	}

	for i, arg0 := range args {
		arg0 = c.cppToksTrim(arg0)
		var arg preprocessingTokens
		for j, v := range arg0 {
			if b := v.Sep(); len(b) != 0 {
				switch {
				case j == 0:
					v.Set(nil, v.Src())
				default:
					w := v
					w.Ch = ' '
					w.len = uint32(len(b))
					w.src = w.sep
					w.sep = w.src
					arg = append(arg, w)
				}
			}
			arg = append(arg, v)
		}
		w := 0
		for i, v := range arg {
			switch v.Ch {
			case ' ':
				if i != 0 {
					switch arg[i-1].Ch {
					case ' ', '#', rune(PPPASTE):
						continue
					}
				}
				if i+1 < len(arg) {
					switch arg[i+1].Ch {
					case rune(PPPASTE):
						continue
					}
				}
			}

			arg[w] = v
			w++
		}
		args[i] = arg[:w]
	}
	return args, tok, true
}

func (c *cpp) cppToksTrim(s preprocessingTokens) preprocessingTokens {
	for len(s) != 0 && s[0].Ch == ' ' {
		s = s[1:]
	}
	for len(s) != 0 && s[len(s)-1].Ch == ' ' {
		s = s[:len(s)-1]
	}
	return s
}

func (c *cpp) hsAdd(HS hideSet, TS preprocessingTokens) (r preprocessingTokens) {
	if len(TS) == 0 {
		// if TS is {} then
		//	return {};
		return r
	}

	// note TS must be T^HS’ • TS’
	// return T^(HS∪HS’) • hsadd(HS,TS’);
	for _, v := range TS {
		v.hs = v.hs.cup(HS)
		r = append(r, v)
	}
	return r
}

// c returns the current token rune the preprocessor is positioned on.
func (c *cpp) c() rune {
	if c.tok.Ch != eof || c.closed {
		return c.tok.Ch
	}

	if c.tok = c.tokenizer.token(); c.tok.Ch == eof {
		c.closed = true
	}
	return c.tok.Ch
}
