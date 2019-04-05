// Code generated by golex. DO NOT EDIT.

// Copyright 2019 The CC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package cc // import "modernc.org/cc/v3"

func (s *scanner) scan() (r rune) {

yystate0:
	yyrule := -1
	_ = yyrule
	c := s.initScan()

	goto yystart1

	goto yystate0 // silence unused label error
	goto yyAction // silence unused label error
yyAction:
	switch yyrule {
	case 1:
		goto yyrule1
	case 2:
		goto yyrule2
	case 3:
		goto yyrule3
	case 4:
		goto yyrule4
	case 5:
		goto yyrule5
	case 6:
		goto yyrule6
	case 7:
		goto yyrule7
	case 8:
		goto yyrule8
	case 9:
		goto yyrule9
	case 10:
		goto yyrule10
	case 11:
		goto yyrule11
	case 12:
		goto yyrule12
	case 13:
		goto yyrule13
	case 14:
		goto yyrule14
	case 15:
		goto yyrule15
	case 16:
		goto yyrule16
	case 17:
		goto yyrule17
	case 18:
		goto yyrule18
	case 19:
		goto yyrule19
	case 20:
		goto yyrule20
	case 21:
		goto yyrule21
	case 22:
		goto yyrule22
	case 23:
		goto yyrule23
	case 24:
		goto yyrule24
	case 25:
		goto yyrule25
	case 26:
		goto yyrule26
	case 27:
		goto yyrule27
	case 28:
		goto yyrule28
	case 29:
		goto yyrule29
	case 30:
		goto yyrule30
	case 31:
		goto yyrule31
	case 32:
		goto yyrule32
	case 33:
		goto yyrule33
	case 34:
		goto yyrule34
	case 35:
		goto yyrule35
	case 36:
		goto yyrule36
	case 37:
		goto yyrule37
	case 38:
		goto yyrule38
	case 39:
		goto yyrule39
	}
	goto yystate1 // silence unused label error
yystate1:
	c = s.next()
yystart1:
	switch {
	default:
		goto yyabort
	case c == '!':
		goto yystate16
	case c == '"':
		goto yystate18
	case c == '#':
		goto yystate29
	case c == '%':
		goto yystate31
	case c == '&':
		goto yystate37
	case c == '*':
		goto yystate52
	case c == '+':
		goto yystate54
	case c == '-':
		goto yystate57
	case c == '.':
		goto yystate61
	case c == '/':
		goto yystate66
	case c == ':':
		goto yystate69
	case c == '<':
		goto yystate71
	case c == '=':
		goto yystate77
	case c == '>':
		goto yystate79
	case c == 'L':
		goto yystate84
	case c == '\'':
		goto yystate40
	case c == '\n':
		goto yystate14
	case c == '\r':
		goto yystate15
	case c == '\t' || c == '\v' || c == '\f' || c == ' ':
		goto yystate2
	case c == '^':
		goto yystate108
	case c == '|':
		goto yystate110
	case c >= '0' && c <= '9':
		goto yystate64
	case c >= 'A' && c <= 'K' || c >= 'M' && c <= 'Z' || c == '_' || c >= 'a' && c <= 'z' || c == '\u0081':
		goto yystate83
	}

yystate2:
	c = s.next()
	yyrule = 2
	s.mark = len(s.charBuf)
	switch {
	default:
		goto yyrule2
	case c == '/':
		goto yystate3
	case c == '\t' || c == '\v' || c == '\f' || c == ' ':
		goto yystate2
	}

yystate3:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '*':
		goto yystate4
	case c == '/':
		goto yystate13
	}

yystate4:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '*':
		goto yystate6
	case c == '\n':
		goto yystate5
	case c >= '\x01' && c <= '\t' || c >= '\v' && c <= ')' || c >= '+' && c <= '\u007f' || c >= '\u0081' && c <= 'ÿ':
		goto yystate4
	}

yystate5:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '*':
		goto yystate6
	case c == '\n':
		goto yystate5
	case c == '\u0080':
		goto yystate7
	case c >= '\x01' && c <= '\t' || c >= '\v' && c <= ')' || c >= '+' && c <= '\u007f' || c >= '\u0081' && c <= 'ÿ':
		goto yystate4
	}

yystate6:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '*':
		goto yystate6
	case c == '/':
		goto yystate2
	case c == '\n':
		goto yystate5
	case c >= '\x01' && c <= '\t' || c >= '\v' && c <= ')' || c >= '+' && c <= '.' || c >= '0' && c <= '\u007f' || c >= '\u0081' && c <= 'ÿ':
		goto yystate4
	}

yystate7:
	c = s.next()
	yyrule = 3
	s.mark = len(s.charBuf)
	switch {
	default:
		goto yyrule3
	case c == '/':
		goto yystate9
	case c == '\t' || c == '\v' || c == '\f' || c == ' ':
		goto yystate8
	}

yystate8:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '/':
		goto yystate9
	case c == '\t' || c == '\v' || c == '\f' || c == ' ':
		goto yystate8
	}

yystate9:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '*':
		goto yystate10
	}

yystate10:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '*':
		goto yystate12
	case c == '\n':
		goto yystate11
	case c >= '\x01' && c <= '\t' || c >= '\v' && c <= ')' || c >= '+' && c <= '\u007f' || c >= '\u0081' && c <= 'ÿ':
		goto yystate10
	}

yystate11:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '*':
		goto yystate12
	case c == '\n':
		goto yystate11
	case c == '\u0080':
		goto yystate7
	case c >= '\x01' && c <= '\t' || c >= '\v' && c <= ')' || c >= '+' && c <= '\u007f' || c >= '\u0081' && c <= 'ÿ':
		goto yystate10
	}

yystate12:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '*':
		goto yystate12
	case c == '/':
		goto yystate8
	case c == '\n':
		goto yystate11
	case c >= '\x01' && c <= '\t' || c >= '\v' && c <= ')' || c >= '+' && c <= '.' || c >= '0' && c <= '\u007f' || c >= '\u0081' && c <= 'ÿ':
		goto yystate10
	}

yystate13:
	c = s.next()
	yyrule = 1
	s.mark = len(s.charBuf)
	switch {
	default:
		goto yyrule1
	case c >= '\x01' && c <= '\t' || c >= '\v' && c <= '\u007f' || c >= '\u0081' && c <= 'ÿ':
		goto yystate13
	}

yystate14:
	c = s.next()
	yyrule = 39
	s.mark = len(s.charBuf)
	goto yyrule39

yystate15:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '\n':
		goto yystate14
	}

yystate16:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '=':
		goto yystate17
	}

yystate17:
	c = s.next()
	yyrule = 4
	s.mark = len(s.charBuf)
	goto yyrule4

yystate18:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '"':
		goto yystate19
	case c == '\\':
		goto yystate20
	case c >= '\x01' && c <= '\t' || c >= '\v' && c <= '!' || c >= '#' && c <= '[' || c >= ']' && c <= '\u007f' || c >= '\u0081' && c <= 'ÿ':
		goto yystate18
	}

yystate19:
	c = s.next()
	yyrule = 38
	s.mark = len(s.charBuf)
	goto yyrule38

yystate20:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '"' || c == '\'' || c >= '0' && c <= '7' || c == '?' || c == '\\' || c == 'a' || c == 'b' || c == 'f' || c == 'n' || c == 'r' || c == 't' || c == 'v':
		goto yystate18
	case c == 'U':
		goto yystate21
	case c == 'u':
		goto yystate25
	case c == 'x':
		goto yystate28
	}

yystate21:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate22
	}

yystate22:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate23
	}

yystate23:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate24
	}

yystate24:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate25
	}

yystate25:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate26
	}

yystate26:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate27
	}

yystate27:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate28
	}

yystate28:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate18
	}

yystate29:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '#':
		goto yystate30
	}

yystate30:
	c = s.next()
	yyrule = 5
	s.mark = len(s.charBuf)
	goto yyrule5

yystate31:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == ':':
		goto yystate32
	case c == '=':
		goto yystate35
	case c == '>':
		goto yystate36
	}

yystate32:
	c = s.next()
	yyrule = 6
	s.mark = len(s.charBuf)
	switch {
	default:
		goto yyrule6
	case c == '%':
		goto yystate33
	}

yystate33:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == ':':
		goto yystate34
	}

yystate34:
	c = s.next()
	yyrule = 7
	s.mark = len(s.charBuf)
	goto yyrule7

yystate35:
	c = s.next()
	yyrule = 8
	s.mark = len(s.charBuf)
	goto yyrule8

yystate36:
	c = s.next()
	yyrule = 9
	s.mark = len(s.charBuf)
	goto yyrule9

yystate37:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '&':
		goto yystate38
	case c == '=':
		goto yystate39
	}

yystate38:
	c = s.next()
	yyrule = 10
	s.mark = len(s.charBuf)
	goto yyrule10

yystate39:
	c = s.next()
	yyrule = 11
	s.mark = len(s.charBuf)
	goto yyrule11

yystate40:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '\\':
		goto yystate43
	case c >= '\x01' && c <= '\t' || c >= '\v' && c <= '&' || c >= '(' && c <= '[' || c >= ']' && c <= '\u007f' || c >= '\u0081' && c <= 'ÿ':
		goto yystate41
	}

yystate41:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '\'':
		goto yystate42
	case c == '\\':
		goto yystate43
	case c >= '\x01' && c <= '\t' || c >= '\v' && c <= '&' || c >= '(' && c <= '[' || c >= ']' && c <= '\u007f' || c >= '\u0081' && c <= 'ÿ':
		goto yystate41
	}

yystate42:
	c = s.next()
	yyrule = 35
	s.mark = len(s.charBuf)
	goto yyrule35

yystate43:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '"' || c == '\'' || c >= '0' && c <= '7' || c == '?' || c == '\\' || c == 'a' || c == 'b' || c == 'f' || c == 'n' || c == 'r' || c == 't' || c == 'v':
		goto yystate41
	case c == 'U':
		goto yystate44
	case c == 'u':
		goto yystate48
	case c == 'x':
		goto yystate51
	}

yystate44:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate45
	}

yystate45:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate46
	}

yystate46:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate47
	}

yystate47:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate48
	}

yystate48:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate49
	}

yystate49:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate50
	}

yystate50:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate51
	}

yystate51:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate41
	}

yystate52:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '=':
		goto yystate53
	}

yystate53:
	c = s.next()
	yyrule = 12
	s.mark = len(s.charBuf)
	goto yyrule12

yystate54:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '+':
		goto yystate55
	case c == '=':
		goto yystate56
	}

yystate55:
	c = s.next()
	yyrule = 13
	s.mark = len(s.charBuf)
	goto yyrule13

yystate56:
	c = s.next()
	yyrule = 14
	s.mark = len(s.charBuf)
	goto yyrule14

yystate57:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '-':
		goto yystate58
	case c == '=':
		goto yystate59
	case c == '>':
		goto yystate60
	}

yystate58:
	c = s.next()
	yyrule = 15
	s.mark = len(s.charBuf)
	goto yyrule15

yystate59:
	c = s.next()
	yyrule = 16
	s.mark = len(s.charBuf)
	goto yyrule16

yystate60:
	c = s.next()
	yyrule = 17
	s.mark = len(s.charBuf)
	goto yyrule17

yystate61:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '.':
		goto yystate62
	case c >= '0' && c <= '9':
		goto yystate64
	}

yystate62:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '.':
		goto yystate63
	}

yystate63:
	c = s.next()
	yyrule = 18
	s.mark = len(s.charBuf)
	goto yyrule18

yystate64:
	c = s.next()
	yyrule = 37
	s.mark = len(s.charBuf)
	switch {
	default:
		goto yyrule37
	case c == '.' || c >= '0' && c <= '9' || c >= 'A' && c <= 'D' || c >= 'F' && c <= 'O' || c >= 'Q' && c <= 'Z' || c == '_' || c >= 'a' && c <= 'd' || c >= 'f' && c <= 'o' || c >= 'q' && c <= 'z' || c == '\u0081':
		goto yystate64
	case c == 'E' || c == 'P' || c == 'e' || c == 'p':
		goto yystate65
	}

yystate65:
	c = s.next()
	yyrule = 37
	s.mark = len(s.charBuf)
	switch {
	default:
		goto yyrule37
	case c == '+' || c == '-' || c == '.' || c >= '0' && c <= '9' || c >= 'A' && c <= 'D' || c >= 'F' && c <= 'O' || c >= 'Q' && c <= 'Z' || c == '_' || c >= 'a' && c <= 'd' || c >= 'f' && c <= 'o' || c >= 'q' && c <= 'z' || c == '\u0081':
		goto yystate64
	case c == 'E' || c == 'P' || c == 'e' || c == 'p':
		goto yystate65
	}

yystate66:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '*':
		goto yystate4
	case c == '/':
		goto yystate67
	case c == '=':
		goto yystate68
	}

yystate67:
	c = s.next()
	yyrule = 1
	s.mark = len(s.charBuf)
	switch {
	default:
		goto yyrule1
	case c >= '\x01' && c <= '\t' || c >= '\v' && c <= '\u007f' || c >= '\u0081' && c <= 'ÿ':
		goto yystate67
	}

yystate68:
	c = s.next()
	yyrule = 19
	s.mark = len(s.charBuf)
	goto yyrule19

yystate69:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '>':
		goto yystate70
	}

yystate70:
	c = s.next()
	yyrule = 20
	s.mark = len(s.charBuf)
	goto yyrule20

yystate71:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '%':
		goto yystate72
	case c == ':':
		goto yystate73
	case c == '<':
		goto yystate74
	case c == '=':
		goto yystate76
	}

yystate72:
	c = s.next()
	yyrule = 21
	s.mark = len(s.charBuf)
	goto yyrule21

yystate73:
	c = s.next()
	yyrule = 22
	s.mark = len(s.charBuf)
	goto yyrule22

yystate74:
	c = s.next()
	yyrule = 23
	s.mark = len(s.charBuf)
	switch {
	default:
		goto yyrule23
	case c == '=':
		goto yystate75
	}

yystate75:
	c = s.next()
	yyrule = 24
	s.mark = len(s.charBuf)
	goto yyrule24

yystate76:
	c = s.next()
	yyrule = 25
	s.mark = len(s.charBuf)
	goto yyrule25

yystate77:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '=':
		goto yystate78
	}

yystate78:
	c = s.next()
	yyrule = 26
	s.mark = len(s.charBuf)
	goto yyrule26

yystate79:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '=':
		goto yystate80
	case c == '>':
		goto yystate81
	}

yystate80:
	c = s.next()
	yyrule = 27
	s.mark = len(s.charBuf)
	goto yyrule27

yystate81:
	c = s.next()
	yyrule = 28
	s.mark = len(s.charBuf)
	switch {
	default:
		goto yyrule28
	case c == '=':
		goto yystate82
	}

yystate82:
	c = s.next()
	yyrule = 29
	s.mark = len(s.charBuf)
	goto yyrule29

yystate83:
	c = s.next()
	yyrule = 36
	s.mark = len(s.charBuf)
	switch {
	default:
		goto yyrule36
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'Z' || c == '_' || c >= 'a' && c <= 'z' || c == '\u0081':
		goto yystate83
	}

yystate84:
	c = s.next()
	yyrule = 36
	s.mark = len(s.charBuf)
	switch {
	default:
		goto yyrule36
	case c == '"':
		goto yystate85
	case c == '\'':
		goto yystate96
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'Z' || c == '_' || c >= 'a' && c <= 'z' || c == '\u0081':
		goto yystate83
	}

yystate85:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '"':
		goto yystate86
	case c == '\\':
		goto yystate87
	case c >= '\x01' && c <= '\t' || c >= '\v' && c <= '!' || c >= '#' && c <= '[' || c >= ']' && c <= '\u007f' || c >= '\u0081' && c <= 'ÿ':
		goto yystate85
	}

yystate86:
	c = s.next()
	yyrule = 33
	s.mark = len(s.charBuf)
	goto yyrule33

yystate87:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '"' || c == '\'' || c >= '0' && c <= '7' || c == '?' || c == '\\' || c == 'a' || c == 'b' || c == 'f' || c == 'n' || c == 'r' || c == 't' || c == 'v':
		goto yystate85
	case c == 'U':
		goto yystate88
	case c == 'u':
		goto yystate92
	case c == 'x':
		goto yystate95
	}

yystate88:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate89
	}

yystate89:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate90
	}

yystate90:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate91
	}

yystate91:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate92
	}

yystate92:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate93
	}

yystate93:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate94
	}

yystate94:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate95
	}

yystate95:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate85
	}

yystate96:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '\\':
		goto yystate99
	case c >= '\x01' && c <= '\t' || c >= '\v' && c <= '&' || c >= '(' && c <= '[' || c >= ']' && c <= '\u007f' || c >= '\u0081' && c <= 'ÿ':
		goto yystate97
	}

yystate97:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '\'':
		goto yystate98
	case c == '\\':
		goto yystate99
	case c >= '\x01' && c <= '\t' || c >= '\v' && c <= '&' || c >= '(' && c <= '[' || c >= ']' && c <= '\u007f' || c >= '\u0081' && c <= 'ÿ':
		goto yystate97
	}

yystate98:
	c = s.next()
	yyrule = 34
	s.mark = len(s.charBuf)
	goto yyrule34

yystate99:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '"' || c == '\'' || c >= '0' && c <= '7' || c == '?' || c == '\\' || c == 'a' || c == 'b' || c == 'f' || c == 'n' || c == 'r' || c == 't' || c == 'v':
		goto yystate97
	case c == 'U':
		goto yystate100
	case c == 'u':
		goto yystate104
	case c == 'x':
		goto yystate107
	}

yystate100:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate101
	}

yystate101:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate102
	}

yystate102:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate103
	}

yystate103:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate104
	}

yystate104:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate105
	}

yystate105:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate106
	}

yystate106:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate107
	}

yystate107:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c >= '0' && c <= '9' || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f':
		goto yystate97
	}

yystate108:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '=':
		goto yystate109
	}

yystate109:
	c = s.next()
	yyrule = 30
	s.mark = len(s.charBuf)
	goto yyrule30

yystate110:
	c = s.next()
	switch {
	default:
		goto yyabort
	case c == '=':
		goto yystate111
	case c == '|':
		goto yystate112
	}

yystate111:
	c = s.next()
	yyrule = 31
	s.mark = len(s.charBuf)
	goto yyrule31

yystate112:
	c = s.next()
	yyrule = 32
	s.mark = len(s.charBuf)
	goto yyrule32

yyrule1: // ({white-space}|{comment})*{line-comment}
yyrule2: // ({white-space}|{comment})+{line-comment}?
	{

		return ' '
	}
yyrule3: // (({white-space}|{comment})*{comment-not-terminated})+
	{

		return s.unterminatedComment()
	}
yyrule4: // "!="
	{
		return NEQ
	}
yyrule5: // "##"
	{
		return PPPASTE
	}
yyrule6: // "%:"
	{
		return '#'
	}
yyrule7: // "%:%:"
	{
		return PPPASTE
	}
yyrule8: // "%="
	{
		return MODASSIGN
	}
yyrule9: // "%>"
	{
		return '}'
	}
yyrule10: // "&&"
	{
		return ANDAND
	}
yyrule11: // "&="
	{
		return ANDASSIGN
	}
yyrule12: // "*="
	{
		return MULASSIGN
	}
yyrule13: // "++"
	{
		return INC
	}
yyrule14: // "+="
	{
		return ADDASSIGN
	}
yyrule15: // "--"
	{
		return DEC
	}
yyrule16: // "-="
	{
		return SUBASSIGN
	}
yyrule17: // "->"
	{
		return ARROW
	}
yyrule18: // "..."
	{
		return DDD
	}
yyrule19: // "/="
	{
		return DIVASSIGN
	}
yyrule20: // ":>"
	{
		return ']'
	}
yyrule21: // "<%"
	{
		return '{'
	}
yyrule22: // "<:"
	{
		return '['
	}
yyrule23: // "<<"
	{
		return LSH
	}
yyrule24: // "<<="
	{
		return LSHASSIGN
	}
yyrule25: // "<="
	{
		return LEQ
	}
yyrule26: // "=="
	{
		return EQ
	}
yyrule27: // ">="
	{
		return GEQ
	}
yyrule28: // ">>"
	{
		return RSH
	}
yyrule29: // ">>="
	{
		return RSHASSIGN
	}
yyrule30: // "^="
	{
		return XORASSIGN
	}
yyrule31: // "|="
	{
		return ORASSIGN
	}
yyrule32: // "||"
	{
		return OROR
	}
yyrule33: // L{string-literal}
	{
		return LONGSTRINGLITERAL
	}
yyrule34: // L{character-constant}
	{
		return LONGCHARCONST
	}
yyrule35: // {character-constant}
	{
		return CHARCONST
	}
yyrule36: // {identifier}
	{
		return IDENTIFIER
	}
yyrule37: // {pp-number}
	{
		return PPNUMBER
	}
yyrule38: // {string-literal}
	{
		return STRINGLITERAL
	}
yyrule39: // \r?\n
	{
		return '\n'
	}
	panic("unreachable")

	goto yyabort // silence unused label error

yyabort: // no lexem recognized
	if c, ok := s.abort(); ok {
		return rune(c)
	}

	goto yyAction
}
