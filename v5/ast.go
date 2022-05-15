// Copyright 2022 The CC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package cc

import (
	"fmt"

	"modernc.org/token"
)

// AbstractDeclaratorCase represents case numbers of production AbstractDeclarator
type AbstractDeclaratorCase int

// Values of type AbstractDeclaratorCase
const (
	AbstractDeclaratorPtr AbstractDeclaratorCase = iota
	AbstractDeclaratorDecl
)

// String implements fmt.Stringer
func (n AbstractDeclaratorCase) String() string {
	switch n {
	case AbstractDeclaratorPtr:
		return "AbstractDeclaratorPtr"
	case AbstractDeclaratorDecl:
		return "AbstractDeclaratorDecl"
	default:
		return fmt.Sprintf("AbstractDeclaratorCase(%v)", int(n))
	}
}

// AbstractDeclarator represents data reduced by productions:
//
//	AbstractDeclarator:
//	        Pointer                           // Case AbstractDeclaratorPtr
//	|       Pointer DirectAbstractDeclarator  // Case AbstractDeclaratorDecl
type AbstractDeclarator struct {
	typer
	Case                     AbstractDeclaratorCase `PrettyPrint:"stringer,zero"`
	DirectAbstractDeclarator *DirectAbstractDeclarator
	Pointer                  *Pointer
}

// String implements fmt.Stringer.
func (n *AbstractDeclarator) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *AbstractDeclarator) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 0:
		return n.Pointer.Position()
	case 1:
		if p := n.Pointer.Position(); p.IsValid() {
			return p
		}

		return n.DirectAbstractDeclarator.Position()
	default:
		panic("internal error")
	}
}

type BinaryOperation int

const (
	BinaryOperationAdd = BinaryOperation(iota + 1)
	BinaryOperationSub
	BinaryOperationMul
	BinaryOperationDiv
	BinaryOperationMod
	BinaryOperationOr
	BinaryOperationAnd
	BinaryOperationXor
	BinaryOperationLsh
	BinaryOperationRsh
	BinaryOperationEq
	BinaryOperationNeq
	BinaryOperationLt
	BinaryOperationGt
	BinaryOperationLeq
	BinaryOperationGeq
	BinaryOperationLOr
	BinaryOperationLAnd
)

type BinaryExpression struct {
	typer
	valuer
	Lhs   ExpressionNode
	Op    BinaryOperation `PrettyPrint:"stringer,zero"`
	Token Token
	Rhs   ExpressionNode
}

// String implements fmt.Stringer.
func (n *BinaryExpression) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *BinaryExpression) Position() (r token.Position) {
	if n == nil {
		return r
	}
	if p := n.Lhs.Position(); p.IsValid() {
		return p
	}

	if p := n.Token.Position(); p.IsValid() {
		return p
	}

	return n.Rhs.Position()
}

// AlignmentSpecifierCase represents case numbers of production AlignmentSpecifier
type AlignmentSpecifierCase int

// Values of type AlignmentSpecifierCase
const (
	AlignmentSpecifierType AlignmentSpecifierCase = iota
	AlignmentSpecifierExpr
)

// String implements fmt.Stringer
func (n AlignmentSpecifierCase) String() string {
	switch n {
	case AlignmentSpecifierType:
		return "AlignmentSpecifierType"
	case AlignmentSpecifierExpr:
		return "AlignmentSpecifierExpr"
	default:
		return fmt.Sprintf("AlignmentSpecifierCase(%v)", int(n))
	}
}

// AlignmentSpecifier represents data reduced by productions:
//
//	AlignmentSpecifier:
//	        "_Alignas" '(' TypeName ')'            // Case AlignmentSpecifierType
//	|       "_Alignas" '(' ConstantExpression ')'  // Case AlignmentSpecifierExpr
type AlignmentSpecifier struct {
	Case               AlignmentSpecifierCase `PrettyPrint:"stringer,zero"`
	ConstantExpression ExpressionNode
	Token              Token
	Token2             Token
	Token3             Token
	TypeName           *TypeName
}

// String implements fmt.Stringer.
func (n *AlignmentSpecifier) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *AlignmentSpecifier) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 1:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.ConstantExpression.Position(); p.IsValid() {
			return p
		}

		return n.Token3.Position()
	case 0:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.TypeName.Position(); p.IsValid() {
			return p
		}

		return n.Token3.Position()
	default:
		panic("internal error")
	}
}

// ArgumentExpressionList represents data reduced by productions:
//
//	ArgumentExpressionList:
//	        AssignmentExpression
//	|       ArgumentExpressionList ',' AssignmentExpression
type ArgumentExpressionList struct {
	ArgumentExpressionList *ArgumentExpressionList
	AssignmentExpression   ExpressionNode
	Token                  Token
}

// String implements fmt.Stringer.
func (n *ArgumentExpressionList) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *ArgumentExpressionList) Position() (r token.Position) {
	if n == nil {
		return r
	}

	return n.AssignmentExpression.Position()
}

// Asm represents data reduced by production:
//
//	Asm:
//	        "__asm__" AsmQualifierList '(' STRINGLITERAL AsmArgList ')'
type Asm struct {
	AsmArgList       *AsmArgList
	AsmQualifierList *AsmQualifierList
	Token            Token
	Token2           Token
	Token3           Token
	Token4           Token
}

// String implements fmt.Stringer.
func (n *Asm) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *Asm) Position() (r token.Position) {
	if n == nil {
		return r
	}

	if p := n.Token.Position(); p.IsValid() {
		return p
	}

	if p := n.AsmQualifierList.Position(); p.IsValid() {
		return p
	}

	if p := n.Token2.Position(); p.IsValid() {
		return p
	}

	if p := n.Token3.Position(); p.IsValid() {
		return p
	}

	if p := n.AsmArgList.Position(); p.IsValid() {
		return p
	}

	return n.Token4.Position()
}

// AsmArgList represents data reduced by productions:
//
//	AsmArgList:
//	        ':' AsmExpressionList
//	|       AsmArgList ':' AsmExpressionList
type AsmArgList struct {
	AsmArgList        *AsmArgList
	AsmExpressionList *AsmExpressionList
	Token             Token
}

// String implements fmt.Stringer.
func (n *AsmArgList) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *AsmArgList) Position() (r token.Position) {
	if n == nil {
		return r
	}

	if p := n.Token.Position(); p.IsValid() {
		return p
	}

	return n.AsmExpressionList.Position()
}

// AsmExpressionList represents data reduced by productions:
//
//	AsmExpressionList:
//	        AsmIndex AssignmentExpression
//	|       AsmExpressionList ',' AsmIndex AssignmentExpression
type AsmExpressionList struct {
	AsmExpressionList    *AsmExpressionList
	AsmIndex             *AsmIndex
	AssignmentExpression ExpressionNode
	Token                Token
}

// String implements fmt.Stringer.
func (n *AsmExpressionList) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *AsmExpressionList) Position() (r token.Position) {
	if n == nil {
		return r
	}

	if p := n.AsmIndex.Position(); p.IsValid() {
		return p
	}

	return n.AssignmentExpression.Position()
}

// AsmIndex represents data reduced by production:
//
//	AsmIndex:
//	        '[' ExpressionList ']'
type AsmIndex struct {
	ExpressionList ExpressionNode
	Token          Token
	Token2         Token
}

// String implements fmt.Stringer.
func (n *AsmIndex) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *AsmIndex) Position() (r token.Position) {
	if n == nil {
		return r
	}

	if p := n.Token.Position(); p.IsValid() {
		return p
	}

	if p := n.ExpressionList.Position(); p.IsValid() {
		return p
	}

	return n.Token2.Position()
}

// AsmQualifierCase represents case numbers of production AsmQualifier
type AsmQualifierCase int

// Values of type AsmQualifierCase
const (
	AsmQualifierVolatile AsmQualifierCase = iota
	AsmQualifierInline
	AsmQualifierGoto
)

// String implements fmt.Stringer
func (n AsmQualifierCase) String() string {
	switch n {
	case AsmQualifierVolatile:
		return "AsmQualifierVolatile"
	case AsmQualifierInline:
		return "AsmQualifierInline"
	case AsmQualifierGoto:
		return "AsmQualifierGoto"
	default:
		return fmt.Sprintf("AsmQualifierCase(%v)", int(n))
	}
}

// AsmQualifier represents data reduced by productions:
//
//	AsmQualifier:
//	        "volatile"  // Case AsmQualifierVolatile
//	|       "inline"    // Case AsmQualifierInline
//	|       "goto"      // Case AsmQualifierGoto
type AsmQualifier struct {
	Case  AsmQualifierCase `PrettyPrint:"stringer,zero"`
	Token Token
}

// String implements fmt.Stringer.
func (n *AsmQualifier) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *AsmQualifier) Position() (r token.Position) {
	if n == nil {
		return r
	}

	return n.Token.Position()
}

// AsmQualifierList represents data reduced by productions:
//
//	AsmQualifierList:
//	        AsmQualifier
//	|       AsmQualifierList AsmQualifier
type AsmQualifierList struct {
	AsmQualifier     *AsmQualifier
	AsmQualifierList *AsmQualifierList
}

// String implements fmt.Stringer.
func (n *AsmQualifierList) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *AsmQualifierList) Position() (r token.Position) {
	if n == nil {
		return r
	}

	return n.AsmQualifier.Position()
}

// AsmStatement represents data reduced by production:
//
//	AsmStatement:
//	        Asm ';'
type AsmStatement struct {
	Asm   *Asm
	Token Token
}

// String implements fmt.Stringer.
func (n *AsmStatement) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *AsmStatement) Position() (r token.Position) {
	if n == nil {
		return r
	}

	if p := n.Asm.Position(); p.IsValid() {
		return p
	}

	return n.Token.Position()
}

type AssignmentOperation int

const (
	AssignmentOperationAssign = AssignmentOperation(iota)
	AssignmentOperationMul
	AssignmentOperationDiv
	AssignmentOperationMod
	AssignmentOperationAdd
	AssignmentOperationSub
	AssignmentOperationLsh
	AssignmentOperationRsh
	AssignmentOperationAnd
	AssignmentOperationXor
	AssignmentOperationOr
)

// AssignmentExpression represents data reduced by productions:
//
//	AssignmentExpression:
//	        ConditionalExpression                       // Case AssignmentExpressionCond
//	|       UnaryExpression '=' AssignmentExpression    // Case AssignmentExpressionAssign
//	|       UnaryExpression "*=" AssignmentExpression   // Case AssignmentExpressionMul
//	|       UnaryExpression "/=" AssignmentExpression   // Case AssignmentExpressionDiv
//	|       UnaryExpression "%=" AssignmentExpression   // Case AssignmentExpressionMod
//	|       UnaryExpression "+=" AssignmentExpression   // Case AssignmentExpressionAdd
//	|       UnaryExpression "-=" AssignmentExpression   // Case AssignmentExpressionSub
//	|       UnaryExpression "<<=" AssignmentExpression  // Case AssignmentExpressionLsh
//	|       UnaryExpression ">>=" AssignmentExpression  // Case AssignmentExpressionRsh
//	|       UnaryExpression "&=" AssignmentExpression   // Case AssignmentExpressionAnd
//	|       UnaryExpression "^=" AssignmentExpression   // Case AssignmentExpressionXor
//	|       UnaryExpression "|=" AssignmentExpression   // Case AssignmentExpressionOr
type AssignmentExpression struct {
	typer
	valuer
	Lhs   ExpressionNode
	Op    AssignmentOperation `PrettyPrint:"stringer,zero"`
	Token Token
	Rhs   ExpressionNode
}

// String implements fmt.Stringer.
func (n *AssignmentExpression) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *AssignmentExpression) Position() (r token.Position) {
	if n == nil {
		return r
	}

	if p := n.Lhs.Position(); p.IsValid() {
		return p
	}

	if p := n.Token.Position(); p.IsValid() {
		return p
	}

	return n.Rhs.Position()
}

// AtomicTypeSpecifier represents data reduced by production:
//
//	AtomicTypeSpecifier:
//	        "_Atomic" '(' TypeName ')'
type AtomicTypeSpecifier struct {
	Token    Token
	Token2   Token
	Token3   Token
	TypeName *TypeName
}

// String implements fmt.Stringer.
func (n *AtomicTypeSpecifier) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *AtomicTypeSpecifier) Position() (r token.Position) {
	if n == nil {
		return r
	}

	if p := n.Token.Position(); p.IsValid() {
		return p
	}

	if p := n.Token2.Position(); p.IsValid() {
		return p
	}

	if p := n.TypeName.Position(); p.IsValid() {
		return p
	}

	return n.Token3.Position()
}

// AttributeSpecifier represents data reduced by production:
//
//	AttributeSpecifier:
//	        "__attribute__" '(' '(' AttributeValueList ')' ')'
type AttributeSpecifier struct {
	AttributeValueList *AttributeValueList
	Token              Token
	Token2             Token
	Token3             Token
	Token4             Token
	Token5             Token
}

// String implements fmt.Stringer.
func (n *AttributeSpecifier) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *AttributeSpecifier) Position() (r token.Position) {
	if n == nil {
		return r
	}

	if p := n.Token.Position(); p.IsValid() {
		return p
	}

	if p := n.Token2.Position(); p.IsValid() {
		return p
	}

	if p := n.Token3.Position(); p.IsValid() {
		return p
	}

	if p := n.AttributeValueList.Position(); p.IsValid() {
		return p
	}

	if p := n.Token4.Position(); p.IsValid() {
		return p
	}

	return n.Token5.Position()
}

// AttributeSpecifierList represents data reduced by productions:
//
//	AttributeSpecifierList:
//	        AttributeSpecifier
//	|       AttributeSpecifierList AttributeSpecifier
type AttributeSpecifierList struct {
	AttributeSpecifier     *AttributeSpecifier
	AttributeSpecifierList *AttributeSpecifierList
}

// String implements fmt.Stringer.
func (n *AttributeSpecifierList) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *AttributeSpecifierList) Position() (r token.Position) {
	if n == nil {
		return r
	}

	return n.AttributeSpecifier.Position()
}

// AttributeValueCase represents case numbers of production AttributeValue
type AttributeValueCase int

// Values of type AttributeValueCase
const (
	AttributeValueIdent AttributeValueCase = iota
	AttributeValueExpr
)

// String implements fmt.Stringer
func (n AttributeValueCase) String() string {
	switch n {
	case AttributeValueIdent:
		return "AttributeValueIdent"
	case AttributeValueExpr:
		return "AttributeValueExpr"
	default:
		return fmt.Sprintf("AttributeValueCase(%v)", int(n))
	}
}

// AttributeValue represents data reduced by productions:
//
//	AttributeValue:
//	        IDENTIFIER                                 // Case AttributeValueIdent
//	|       IDENTIFIER '(' ArgumentExpressionList ')'  // Case AttributeValueExpr
type AttributeValue struct {
	ArgumentExpressionList *ArgumentExpressionList
	Case                   AttributeValueCase `PrettyPrint:"stringer,zero"`
	Token                  Token
	Token2                 Token
	Token3                 Token
}

// String implements fmt.Stringer.
func (n *AttributeValue) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *AttributeValue) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 0:
		return n.Token.Position()
	case 1:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.ArgumentExpressionList.Position(); p.IsValid() {
			return p
		}

		return n.Token3.Position()
	default:
		panic("internal error")
	}
}

// AttributeValueList represents data reduced by productions:
//
//	AttributeValueList:
//	        AttributeValue
//	|       AttributeValueList ',' AttributeValue
type AttributeValueList struct {
	AttributeValue     *AttributeValue
	AttributeValueList *AttributeValueList
	Token              Token
}

// String implements fmt.Stringer.
func (n *AttributeValueList) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *AttributeValueList) Position() (r token.Position) {
	if n == nil {
		return r
	}

	return n.AttributeValue.Position()
}

// BlockItem represents data reduced by productions:
//
//	BlockItem:
//	        Declaration                                         // Case BlockItemDecl
//	|       LabelDeclaration                                    // Case BlockItemLabel
//	|       Statement                                           // Case BlockItemStmt
//	|       DeclarationSpecifiers Declarator CompoundStatement  // Case BlockItemFuncDef
type BlockItem interface {
	Node
	fmt.Stringer
	isBlockItem()
	check(c *ctx) Type
}

func (*LabelDeclaration) isBlockItem()        {}
func (*FunctionDefinition) isBlockItem()      {}
func (*CommonDeclaration) isBlockItem()       {}
func (*StaticAssertDeclaration) isBlockItem() {}
func (*AutoDeclaration) isBlockItem()         {}

func (*LabeledStatement) isBlockItem()    {}
func (*CompoundStatement) isBlockItem()   {}
func (*ExpressionStatement) isBlockItem() {}
func (*SelectionStatement) isBlockItem()  {}
func (*IterationStatement) isBlockItem()  {}
func (*JumpStatement) isBlockItem()       {}
func (*AsmStatement) isBlockItem()        {}

// CastExpression represents data reduced by productions:
//
//	CastExpression:
//	        UnaryExpression                  // Case CastExpressionUnary
//	|       '(' TypeName ')' CastExpression  // Case CastExpressionCast
type CastExpression struct {
	typer
	valuer
	Lparen   Token
	TypeName *TypeName
	Rparen   Token
	Expr     ExpressionNode
}

// String implements fmt.Stringer.
func (n *CastExpression) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *CastExpression) Position() (r token.Position) {
	if n == nil {
		return r
	}

	if p := n.Lparen.Position(); p.IsValid() {
		return p
	}

	if p := n.TypeName.Position(); p.IsValid() {
		return p
	}

	if p := n.Rparen.Position(); p.IsValid() {
		return p
	}

	return n.Expr.Position()
}

// CompoundStatement represents data reduced by production:
//
//	CompoundStatement:
//	        '{' BlockItemList '}'
type CompoundStatement struct {
	lexicalScoper
	Lbrace Token
	List   []BlockItem
	Rbrace Token
}

// String implements fmt.Stringer.
func (n *CompoundStatement) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *CompoundStatement) Position() (r token.Position) {
	if n == nil {
		return r
	}
	if p := n.Lbrace.Position(); p.IsValid() {
		return p
	}

	for _, v := range n.List {
		if p := v.Position(); p.IsValid() {
			return p
		}
	}

	return n.Rbrace.Position()
}

// ConditionalExpression represents data reduced by productions:
//
//	ConditionalExpression:
//	        LogicalOrExpression                                               // Case ConditionalExpressionLOr
//	|       LogicalOrExpression '?' ExpressionList ':' ConditionalExpression  // Case ConditionalExpressionCond
type ConditionalExpression struct {
	typer
	valuer
	Condition ExpressionNode
	Token     Token
	Then      ExpressionNode
	Token2    Token
	Else      ExpressionNode
}

// String implements fmt.Stringer.
func (n *ConditionalExpression) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *ConditionalExpression) Position() (r token.Position) {
	if n == nil {
		return r
	}

	if p := n.Condition.Position(); p.IsValid() {
		return p
	}

	if p := n.Token.Position(); p.IsValid() {
		return p
	}

	if p := n.Then.Position(); p.IsValid() {
		return p
	}

	if p := n.Token2.Position(); p.IsValid() {
		return p
	}

	return n.Else.Position()
}

// ConstantExpression represents data reduced by production:
//
//	ConstantExpression:
//	        ConditionalExpression
type ConstantExpression struct {
	typer
	valuer
	ConditionalExpression ExpressionNode
}

// String implements fmt.Stringer.
func (n *ConstantExpression) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *ConstantExpression) Position() (r token.Position) {
	if n == nil {
		return r
	}

	return n.ConditionalExpression.Position()
}

// Declaration represents data reduced by productions:
//
//	Declaration:
//	        DeclarationSpecifiers InitDeclaratorList AttributeSpecifierList ';'  // Case DeclarationDecl
//	|       StaticAssertDeclaration                                              // Case DeclarationAssert
//	|       "__auto_type" Declarator '=' Initializer ';'                         // Case DeclarationAuto
type Declaration interface {
	Node
	fmt.Stringer
	BlockItem
	ExternalDeclaration
	isDeclaration()
}

func (*CommonDeclaration) isDeclaration()       {}
func (*StaticAssertDeclaration) isDeclaration() {}
func (*AutoDeclaration) isDeclaration()         {}

type CommonDeclaration struct {
	DeclarationSpecifiers  DeclarationSpecifiers
	InitDeclaratorList     *InitDeclaratorList
	AttributeSpecifierList *AttributeSpecifierList
	Token                  Token
}

// String implements fmt.Stringer.
func (n *CommonDeclaration) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *CommonDeclaration) Position() (r token.Position) {
	if n == nil {
		return r
	}
	for _, s := range n.DeclarationSpecifiers {
		if p := s.Position(); p.IsValid() {
			return p
		}
	}

	if p := n.InitDeclaratorList.Position(); p.IsValid() {
		return p
	}

	if p := n.AttributeSpecifierList.Position(); p.IsValid() {
		return p
	}

	return n.Token.Position()
}

type AutoDeclaration struct {
	Token       Token
	Declarator  *Declarator
	Token2      Token
	Initializer *Initializer
	Token3      Token
}

// String implements fmt.Stringer.
func (n *AutoDeclaration) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *AutoDeclaration) Position() (r token.Position) {
	if n == nil {
		return r
	}
	if p := n.Token.Position(); p.IsValid() {
		return p
	}

	if p := n.Declarator.Position(); p.IsValid() {
		return p
	}

	if p := n.Token2.Position(); p.IsValid() {
		return p
	}

	if p := n.Initializer.Position(); p.IsValid() {
		return p
	}

	return n.Token3.Position()
}

// DeclarationSpecifier represents data reduced by productions:
//
//	DeclarationSpecifiers:
//	        StorageClassSpecifier DeclarationSpecifiers  // Case DeclarationSpecifiersStorage
//	|       TypeSpecifier DeclarationSpecifiers          // Case DeclarationSpecifiersTypeSpec
//	|       TypeQualifier DeclarationSpecifiers          // Case DeclarationSpecifiersTypeQual
//	|       FunctionSpecifier DeclarationSpecifiers      // Case DeclarationSpecifiersFunc
//	|       AlignmentSpecifier DeclarationSpecifiers     // Case DeclarationSpecifiersAlignSpec
//	|       "__attribute__"                              // Case DeclarationSpecifiersAttr
type DeclarationSpecifier interface {
	Node
	fmt.Stringer
	isDeclarationSpecifier()
}

func (*StorageClassSpecifier) isDeclarationSpecifier()  {}
func (*TypeSpecifier) isDeclarationSpecifier()          {}
func (*TypeQualifier) isDeclarationSpecifier()          {}
func (*FunctionSpecifier) isDeclarationSpecifier()      {}
func (*AlignmentSpecifier) isDeclarationSpecifier()     {}
func (*AttributeSpecifierList) isDeclarationSpecifier() {}

type DeclarationSpecifiers []DeclarationSpecifier

func (list DeclarationSpecifiers) IsTypedef() bool {
	for _, d := range list {
		if s, ok := d.(*StorageClassSpecifier); ok && s.Case == StorageClassSpecifierTypedef {
			return true
		}
	}
	return false
}

// Declarator represents data reduced by production:
//
//	Declarator:
//	        Pointer DirectDeclarator
type Declarator struct {
	alignas int
	read    int
	write   int
	lexicalScoper
	typer
	visible
	resolver
	isAtomic         bool
	isAuto           bool
	isConst          bool
	isExtern         bool
	isFuncDef        bool
	isInline         bool
	isNoreturn       bool
	isParam          bool
	isRegister       bool
	isRestrict       bool
	isStatic         bool
	isThreadLocal    bool
	isTypename       bool
	isVolatile       bool
	addrTaken        bool
	DirectDeclarator *DirectDeclarator
	Pointer          *Pointer
}

// String implements fmt.Stringer.
func (n *Declarator) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *Declarator) Position() (r token.Position) {
	if n == nil {
		return r
	}

	if p := n.Pointer.Position(); p.IsValid() {
		return p
	}

	return n.DirectDeclarator.Position()
}

// AddressTaken reports whether address of n is taken. The result is valid
// after Translate.
func (n *Declarator) AddressTaken() bool { return n.addrTaken }

// ReadCount reports the number of times n is read. The result is valid after
// Translate.
func (n *Declarator) ReadCount() int { return n.read }

// WriteCount reports the number of times n is written. The result is valid
// after Translate.
func (n *Declarator) WriteCount() int { return n.write }

// Name returns the name of n.
func (n *Declarator) Name() string {
	if n == nil {
		return ""
	}

	if dn := n.DirectDeclarator.name(); dn != nil {
		return dn.Token.SrcStr()
	}

	return ""
}

// NameTok returns the name token of n.
func (n *Declarator) NameTok() (r Token) {
	if n == nil {
		return r
	}

	if dn := n.DirectDeclarator.name(); dn != nil {
		return dn.Token
	}

	return r
}

func (n *Declarator) isFn() bool {
	if n == nil {
		return false
	}

	return n.DirectDeclarator.isFn()
}

// Linkage describes linkage of identifiers ([0]6.2.2).
type Linkage int

// Values of type Linkage
const (
	External Linkage = iota
	Internal
	None
)

func (n *Declarator) Linkage() Linkage {
	if n.IsTypename() || n.IsParam() {
		return None
	}

	if n.IsStatic() && n.LexicalScope().Parent == nil {
		return Internal
	}

	if n.IsExtern() || n.LexicalScope().Parent == nil {
		return External
	}

	return None
}

// StorageDuration describes storage duration of objects ([0]6.2.4).
type StorageDuration int

// Values of type StorageDuration
const (
	Static StorageDuration = iota
	Automatic
	Allocated
)

func (n *Declarator) StorageDuration() StorageDuration {
	switch l := n.Linkage(); {
	case l == External || l == Internal || n.IsStatic():
		return Static
	case l == None && !n.IsStatic():
		return Automatic
	}

	panic(todo(""))
}

// IsExtern reports whether the storage class specifier 'extern' was present in
// the declaration of n.
func (n *Declarator) IsExtern() bool { return n.isExtern }

// IsConst reports whether the type qualifier 'const' was present in
// the declaration of n.
func (n *Declarator) IsConst() bool { return n.isConst }

// IsInline reports whether the function specifier 'inline' was present in the
// declaration of n.
func (n *Declarator) IsInline() bool { return n.isInline }

// IsVolatile reports whether the type qualifier 'volatile' was present in
// the declaration of n.
func (n *Declarator) IsVolatile() bool { return n.isVolatile }

// IsRegister reports whether the storage class specifier 'register' was
// present in the declaration of n.
func (n *Declarator) IsRegister() bool { return n.isRegister }

// IsStatic reports whether the storage class specifier 'static' was present in
// the declaration of n.
func (n *Declarator) IsStatic() bool { return n.isStatic }

// IsAtomic reports whether the type specifier '_Atomic' was present in the
// declaration of n.
func (n *Declarator) IsAtomic() bool { return n.isAtomic }

// IsThreadLocal reports whether the storage class specifier '_Thread_local'
// was present in the declaration of n.
func (n *Declarator) IsThreadLocal() bool { return n.isThreadLocal }

// IsTypename reports whether n is a typedef.
func (n *Declarator) IsTypename() bool { return n.isTypename }

// Alignas reports whether the _Alignas specifier was present in the
// declaration specifiers of n, if non-zero.
func (n *Declarator) Alignas() int { return n.alignas }

// IsParam reports whether n is a function paramater.
func (n *Declarator) IsParam() bool { return n.isParam }

// Designation represents data reduced by production:
//
//	Designation:
//	        DesignatorList '='
type Designation struct {
	DesignatorList *DesignatorList
	Token          Token
}

// String implements fmt.Stringer.
func (n *Designation) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *Designation) Position() (r token.Position) {
	if n == nil {
		return r
	}

	if p := n.DesignatorList.Position(); p.IsValid() {
		return p
	}

	return n.Token.Position()
}

// DesignatorCase represents case numbers of production Designator
type DesignatorCase int

// Values of type DesignatorCase
const (
	DesignatorIndex DesignatorCase = iota
	DesignatorIndex2
	DesignatorField
	DesignatorField2
)

// String implements fmt.Stringer
func (n DesignatorCase) String() string {
	switch n {
	case DesignatorIndex:
		return "DesignatorIndex"
	case DesignatorIndex2:
		return "DesignatorIndex2"
	case DesignatorField:
		return "DesignatorField"
	case DesignatorField2:
		return "DesignatorField2"
	default:
		return fmt.Sprintf("DesignatorCase(%v)", int(n))
	}
}

// Designator represents data reduced by productions:
//
//	Designator:
//	        '[' ConstantExpression ']'                           // Case DesignatorIndex
//	|       '[' ConstantExpression "..." ConstantExpression ']'  // Case DesignatorIndex2
//	|       '.' IDENTIFIER                                       // Case DesignatorField
//	|       IDENTIFIER ':'                                       // Case DesignatorField2
type Designator struct {
	Case                DesignatorCase `PrettyPrint:"stringer,zero"`
	ConstantExpression  ExpressionNode
	ConstantExpression2 ExpressionNode
	Token               Token
	Token2              Token
	Token3              Token
}

// String implements fmt.Stringer.
func (n *Designator) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *Designator) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 0:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.ConstantExpression.Position(); p.IsValid() {
			return p
		}

		return n.Token2.Position()
	case 1:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.ConstantExpression.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.ConstantExpression2.Position(); p.IsValid() {
			return p
		}

		return n.Token3.Position()
	case 2, 3:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		return n.Token2.Position()
	default:
		panic("internal error")
	}
}

// DesignatorList represents data reduced by productions:
//
//	DesignatorList:
//	        Designator
//	|       DesignatorList Designator
type DesignatorList struct {
	Designator     *Designator
	DesignatorList *DesignatorList
}

// String implements fmt.Stringer.
func (n *DesignatorList) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *DesignatorList) Position() (r token.Position) {
	if n == nil {
		return r
	}

	return n.Designator.Position()
}

// DirectAbstractDeclaratorCase represents case numbers of production DirectAbstractDeclarator
type DirectAbstractDeclaratorCase int

// Values of type DirectAbstractDeclaratorCase
const (
	DirectAbstractDeclaratorDecl DirectAbstractDeclaratorCase = iota
	DirectAbstractDeclaratorArr
	DirectAbstractDeclaratorStaticArr
	DirectAbstractDeclaratorArrStatic
	DirectAbstractDeclaratorArrStar
	DirectAbstractDeclaratorFunc
)

// String implements fmt.Stringer
func (n DirectAbstractDeclaratorCase) String() string {
	switch n {
	case DirectAbstractDeclaratorDecl:
		return "DirectAbstractDeclaratorDecl"
	case DirectAbstractDeclaratorArr:
		return "DirectAbstractDeclaratorArr"
	case DirectAbstractDeclaratorStaticArr:
		return "DirectAbstractDeclaratorStaticArr"
	case DirectAbstractDeclaratorArrStatic:
		return "DirectAbstractDeclaratorArrStatic"
	case DirectAbstractDeclaratorArrStar:
		return "DirectAbstractDeclaratorArrStar"
	case DirectAbstractDeclaratorFunc:
		return "DirectAbstractDeclaratorFunc"
	default:
		return fmt.Sprintf("DirectAbstractDeclaratorCase(%v)", int(n))
	}
}

// DirectAbstractDeclarator represents data reduced by productions:
//
//	DirectAbstractDeclarator:
//	        '(' AbstractDeclarator ')'                                                     // Case DirectAbstractDeclaratorDecl
//	|       DirectAbstractDeclarator '[' TypeQualifiers AssignmentExpression ']'           // Case DirectAbstractDeclaratorArr
//	|       DirectAbstractDeclarator '[' "static" TypeQualifiers AssignmentExpression ']'  // Case DirectAbstractDeclaratorStaticArr
//	|       DirectAbstractDeclarator '[' TypeQualifiers "static" AssignmentExpression ']'  // Case DirectAbstractDeclaratorArrStatic
//	|       DirectAbstractDeclarator '[' '*' ']'                                           // Case DirectAbstractDeclaratorArrStar
//	|       DirectAbstractDeclarator '(' ParameterTypeList ')'                             // Case DirectAbstractDeclaratorFunc
type DirectAbstractDeclarator struct {
	params                   *Scope
	AbstractDeclarator       *AbstractDeclarator
	AssignmentExpression     ExpressionNode
	Case                     DirectAbstractDeclaratorCase `PrettyPrint:"stringer,zero"`
	DirectAbstractDeclarator *DirectAbstractDeclarator
	ParameterTypeList        *ParameterTypeList
	Token                    Token
	Token2                   Token
	Token3                   Token
	TypeQualifiers           *TypeQualifiers
}

// String implements fmt.Stringer.
func (n *DirectAbstractDeclarator) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *DirectAbstractDeclarator) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 5:
		if p := n.DirectAbstractDeclarator.Position(); p.IsValid() {
			return p
		}

		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.ParameterTypeList.Position(); p.IsValid() {
			return p
		}

		return n.Token2.Position()
	case 4:
		if p := n.DirectAbstractDeclarator.Position(); p.IsValid() {
			return p
		}

		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		return n.Token3.Position()
	case 2:
		if p := n.DirectAbstractDeclarator.Position(); p.IsValid() {
			return p
		}

		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.TypeQualifiers.Position(); p.IsValid() {
			return p
		}

		if p := n.AssignmentExpression.Position(); p.IsValid() {
			return p
		}

		return n.Token3.Position()
	case 1:
		if p := n.DirectAbstractDeclarator.Position(); p.IsValid() {
			return p
		}

		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.TypeQualifiers.Position(); p.IsValid() {
			return p
		}

		if p := n.AssignmentExpression.Position(); p.IsValid() {
			return p
		}

		return n.Token2.Position()
	case 3:
		if p := n.DirectAbstractDeclarator.Position(); p.IsValid() {
			return p
		}

		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.TypeQualifiers.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.AssignmentExpression.Position(); p.IsValid() {
			return p
		}

		return n.Token3.Position()
	case 0:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.AbstractDeclarator.Position(); p.IsValid() {
			return p
		}

		return n.Token2.Position()
	default:
		panic("internal error")
	}
}

// DirectDeclaratorCase represents case numbers of production DirectDeclarator
type DirectDeclaratorCase int

// Values of type DirectDeclaratorCase
const (
	DirectDeclaratorIdent DirectDeclaratorCase = iota
	DirectDeclaratorDecl
	DirectDeclaratorArr
	DirectDeclaratorStaticArr
	DirectDeclaratorArrStatic
	DirectDeclaratorStar
	DirectDeclaratorFuncParam
	DirectDeclaratorFuncIdent
)

// String implements fmt.Stringer
func (n DirectDeclaratorCase) String() string {
	switch n {
	case DirectDeclaratorIdent:
		return "DirectDeclaratorIdent"
	case DirectDeclaratorDecl:
		return "DirectDeclaratorDecl"
	case DirectDeclaratorArr:
		return "DirectDeclaratorArr"
	case DirectDeclaratorStaticArr:
		return "DirectDeclaratorStaticArr"
	case DirectDeclaratorArrStatic:
		return "DirectDeclaratorArrStatic"
	case DirectDeclaratorStar:
		return "DirectDeclaratorStar"
	case DirectDeclaratorFuncParam:
		return "DirectDeclaratorFuncParam"
	case DirectDeclaratorFuncIdent:
		return "DirectDeclaratorFuncIdent"
	default:
		return fmt.Sprintf("DirectDeclaratorCase(%v)", int(n))
	}
}

// DirectDeclarator represents data reduced by productions:
//
//	DirectDeclarator:
//	        IDENTIFIER                                                             // Case DirectDeclaratorIdent
//	|       '(' Declarator ')'                                                     // Case DirectDeclaratorDecl
//	|       DirectDeclarator '[' TypeQualifiers AssignmentExpression ']'           // Case DirectDeclaratorArr
//	|       DirectDeclarator '[' "static" TypeQualifiers AssignmentExpression ']'  // Case DirectDeclaratorStaticArr
//	|       DirectDeclarator '[' TypeQualifiers "static" AssignmentExpression ']'  // Case DirectDeclaratorArrStatic
//	|       DirectDeclarator '[' TypeQualifiers '*' ']'                            // Case DirectDeclaratorStar
//	|       DirectDeclarator '(' ParameterTypeList ')'                             // Case DirectDeclaratorFuncParam
//	|       DirectDeclarator '(' IdentifierList ')'                                // Case DirectDeclaratorFuncIdent
type DirectDeclarator struct {
	params               *Scope
	AssignmentExpression ExpressionNode
	Case                 DirectDeclaratorCase `PrettyPrint:"stringer,zero"`
	Declarator           *Declarator
	DirectDeclarator     *DirectDeclarator
	IdentifierList       *IdentifierList
	ParameterTypeList    *ParameterTypeList
	Token                Token
	Token2               Token
	Token3               Token
	TypeQualifiers       *TypeQualifiers
}

// String implements fmt.Stringer.
func (n *DirectDeclarator) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *DirectDeclarator) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 7:
		if p := n.DirectDeclarator.Position(); p.IsValid() {
			return p
		}

		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.IdentifierList.Position(); p.IsValid() {
			return p
		}

		return n.Token2.Position()
	case 6:
		if p := n.DirectDeclarator.Position(); p.IsValid() {
			return p
		}

		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.ParameterTypeList.Position(); p.IsValid() {
			return p
		}

		return n.Token2.Position()
	case 3:
		if p := n.DirectDeclarator.Position(); p.IsValid() {
			return p
		}

		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.TypeQualifiers.Position(); p.IsValid() {
			return p
		}

		if p := n.AssignmentExpression.Position(); p.IsValid() {
			return p
		}

		return n.Token3.Position()
	case 2:
		if p := n.DirectDeclarator.Position(); p.IsValid() {
			return p
		}

		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.TypeQualifiers.Position(); p.IsValid() {
			return p
		}

		if p := n.AssignmentExpression.Position(); p.IsValid() {
			return p
		}

		return n.Token2.Position()
	case 4:
		if p := n.DirectDeclarator.Position(); p.IsValid() {
			return p
		}

		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.TypeQualifiers.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.AssignmentExpression.Position(); p.IsValid() {
			return p
		}

		return n.Token3.Position()
	case 5:
		if p := n.DirectDeclarator.Position(); p.IsValid() {
			return p
		}

		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.TypeQualifiers.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		return n.Token3.Position()
	case 0:
		return n.Token.Position()
	case 1:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Declarator.Position(); p.IsValid() {
			return p
		}

		return n.Token2.Position()
	default:
		panic("internal error")
	}
}

func (n *DirectDeclarator) name() *DirectDeclarator {
	if n == nil {
		return nil
	}

	switch n.Case {
	case DirectDeclaratorIdent:
		return n
	case DirectDeclaratorDecl:
		return n.Declarator.DirectDeclarator.name()
	default:
		return n.DirectDeclarator.name()
	}
}

func (n *DirectDeclarator) isFn() bool {
	if n == nil {
		return false
	}

	switch n.Case {
	case DirectDeclaratorFuncParam, DirectDeclaratorFuncIdent:
		return true
	}

	return false
}

// EnumSpecifierCase represents case numbers of production EnumSpecifier
type EnumSpecifierCase int

// Values of type EnumSpecifierCase
const (
	EnumSpecifierDef EnumSpecifierCase = iota
	EnumSpecifierTag
)

// String implements fmt.Stringer
func (n EnumSpecifierCase) String() string {
	switch n {
	case EnumSpecifierDef:
		return "EnumSpecifierDef"
	case EnumSpecifierTag:
		return "EnumSpecifierTag"
	default:
		return fmt.Sprintf("EnumSpecifierCase(%v)", int(n))
	}
}

// EnumSpecifier represents data reduced by productions:
//
//	EnumSpecifier:
//	        "enum" IDENTIFIER '{' EnumeratorList ',' '}'  // Case EnumSpecifierDef
//	|       "enum" IDENTIFIER                             // Case EnumSpecifierTag
type EnumSpecifier struct {
	lexicalScoper
	visible
	typer
	Case           EnumSpecifierCase `PrettyPrint:"stringer,zero"`
	EnumeratorList *EnumeratorList
	Token          Token
	Token2         Token
	Token3         Token
	Token4         Token
	Token5         Token
}

// String implements fmt.Stringer.
func (n *EnumSpecifier) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *EnumSpecifier) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 1:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		return n.Token2.Position()
	case 0:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.Token3.Position(); p.IsValid() {
			return p
		}

		if p := n.EnumeratorList.Position(); p.IsValid() {
			return p
		}

		if p := n.Token4.Position(); p.IsValid() {
			return p
		}

		return n.Token5.Position()
	default:
		panic("internal error")
	}
}

// EnumeratorCase represents case numbers of production Enumerator
type EnumeratorCase int

// Values of type EnumeratorCase
const (
	EnumeratorIdent EnumeratorCase = iota
	EnumeratorExpr
)

// String implements fmt.Stringer
func (n EnumeratorCase) String() string {
	switch n {
	case EnumeratorIdent:
		return "EnumeratorIdent"
	case EnumeratorExpr:
		return "EnumeratorExpr"
	default:
		return fmt.Sprintf("EnumeratorCase(%v)", int(n))
	}
}

// Enumerator represents data reduced by productions:
//
//	Enumerator:
//	        IDENTIFIER                         // Case EnumeratorIdent
//	|       IDENTIFIER '=' ConstantExpression  // Case EnumeratorExpr
type Enumerator struct {
	typer
	resolver
	valuer
	visible
	Case               EnumeratorCase `PrettyPrint:"stringer,zero"`
	ConstantExpression ExpressionNode
	Token              Token
	Token2             Token
}

// String implements fmt.Stringer.
func (n *Enumerator) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *Enumerator) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 0:
		return n.Token.Position()
	case 1:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		return n.ConstantExpression.Position()
	default:
		panic("internal error")
	}
}

// EnumeratorList represents data reduced by productions:
//
//	EnumeratorList:
//	        Enumerator
//	|       EnumeratorList ',' Enumerator
type EnumeratorList struct {
	Enumerator     *Enumerator
	EnumeratorList *EnumeratorList
	Token          Token
}

// String implements fmt.Stringer.
func (n *EnumeratorList) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *EnumeratorList) Position() (r token.Position) {
	if n == nil {
		return r
	}

	return n.Enumerator.Position()
}

// ExpressionList represents data reduced by productions:
//
//	ExpressionList:
//	        AssignmentExpression
//	|       ExpressionList ',' AssignmentExpression
type ExpressionList struct {
	typer
	valuer
	List   []ExpressionNode
	Tokens []Token
}

// String implements fmt.Stringer.
func (n *ExpressionList) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *ExpressionList) Position() (r token.Position) {
	if n == nil {
		return r
	}

	for i := range n.List {
		if i < len(n.List) {
			if p := n.List[i].Position(); p.IsValid() {
				return p
			}
		}
		if i < len(n.Tokens) {
			if p := n.Tokens[i].Position(); p.IsValid() {
				return p
			}
		}
	}

	return r
}

// ExpressionStatement represents data reduced by production:
//
//	ExpressionStatement:
//	        ExpressionList ';'
type ExpressionStatement struct {
	AttributeSpecifierList *AttributeSpecifierList
	declarationSpecifiers  DeclarationSpecifiers
	ExpressionList         ExpressionNode
	Token                  Token
}

// String implements fmt.Stringer.
func (n *ExpressionStatement) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *ExpressionStatement) Position() (r token.Position) {
	if n == nil {
		return r
	}

	if n.ExpressionList != nil {
		if p := n.ExpressionList.Position(); p.IsValid() {
			return p
		}
	}

	return n.Token.Position()
}

// ExternalDeclaration represents data reduced by productions:
//
//	ExternalDeclaration:
//	        FunctionDefinition  // Case ExternalDeclarationFuncDef
//	|       Declaration         // Case ExternalDeclarationDecl
//	|       AsmStatement        // Case ExternalDeclarationAsmStmt
//	|       ';'                 // Case ExternalDeclarationEmpty
type ExternalDeclaration interface {
	Node
	fmt.Stringer
	isExternalDeclaration()
	check(c *ctx) Type
}

func (*FunctionDefinition) isExternalDeclaration()      {}
func (*AsmStatement) isExternalDeclaration()            {}
func (*CommonDeclaration) isExternalDeclaration()       {}
func (*StaticAssertDeclaration) isExternalDeclaration() {}
func (*AutoDeclaration) isExternalDeclaration()         {}

// FunctionDefinition represents data reduced by production:
//
//	FunctionDefinition:
//	        DeclarationSpecifiers Declarator DeclarationList CompoundStatement
type FunctionDefinition struct {
	scope        *Scope
	Specifiers   DeclarationSpecifiers
	Declarator   *Declarator
	Declarations []Declaration
	Body         *CompoundStatement
}

// String implements fmt.Stringer.
func (n *FunctionDefinition) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *FunctionDefinition) Position() (r token.Position) {
	if n == nil {
		return r
	}

	for _, s := range n.Specifiers {
		if p := s.Position(); p.IsValid() {
			return p
		}
	}

	if p := n.Declarator.Position(); p.IsValid() {
		return p
	}

	for _, d := range n.Declarations {
		if p := d.Position(); p.IsValid() {
			return p
		}
	}

	return n.Body.Position()
}

// FunctionSpecifierCase represents case numbers of production FunctionSpecifier
type FunctionSpecifierCase int

// Values of type FunctionSpecifierCase
const (
	FunctionSpecifierInline FunctionSpecifierCase = iota
	FunctionSpecifierNoreturn
)

// String implements fmt.Stringer
func (n FunctionSpecifierCase) String() string {
	switch n {
	case FunctionSpecifierInline:
		return "FunctionSpecifierInline"
	case FunctionSpecifierNoreturn:
		return "FunctionSpecifierNoreturn"
	default:
		return fmt.Sprintf("FunctionSpecifierCase(%v)", int(n))
	}
}

// FunctionSpecifier represents data reduced by productions:
//
//	FunctionSpecifier:
//	        "inline"     // Case FunctionSpecifierInline
//	|       "_Noreturn"  // Case FunctionSpecifierNoreturn
type FunctionSpecifier struct {
	Case  FunctionSpecifierCase `PrettyPrint:"stringer,zero"`
	Token Token
}

// String implements fmt.Stringer.
func (n *FunctionSpecifier) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *FunctionSpecifier) Position() (r token.Position) {
	if n == nil {
		return r
	}

	return n.Token.Position()
}

// GenericAssociationCase represents case numbers of production GenericAssociation
type GenericAssociationCase int

// Values of type GenericAssociationCase
const (
	GenericAssociationType GenericAssociationCase = iota
	GenericAssociationDefault
)

// String implements fmt.Stringer
func (n GenericAssociationCase) String() string {
	switch n {
	case GenericAssociationType:
		return "GenericAssociationType"
	case GenericAssociationDefault:
		return "GenericAssociationDefault"
	default:
		return fmt.Sprintf("GenericAssociationCase(%v)", int(n))
	}
}

// GenericAssociation represents data reduced by productions:
//
//	GenericAssociation:
//	        TypeName ':' AssignmentExpression   // Case GenericAssociationType
//	|       "default" ':' AssignmentExpression  // Case GenericAssociationDefault
type GenericAssociation struct {
	AssignmentExpression ExpressionNode
	Case                 GenericAssociationCase `PrettyPrint:"stringer,zero"`
	Token                Token
	Token2               Token
	TypeName             *TypeName
}

// String implements fmt.Stringer.
func (n *GenericAssociation) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *GenericAssociation) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 1:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		return n.AssignmentExpression.Position()
	case 0:
		if p := n.TypeName.Position(); p.IsValid() {
			return p
		}

		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		return n.AssignmentExpression.Position()
	default:
		panic("internal error")
	}
}

// GenericAssociationList represents data reduced by productions:
//
//	GenericAssociationList:
//	        GenericAssociation
//	|       GenericAssociationList ',' GenericAssociation
type GenericAssociationList struct {
	GenericAssociation     *GenericAssociation
	GenericAssociationList *GenericAssociationList
	Token                  Token
}

// String implements fmt.Stringer.
func (n *GenericAssociationList) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *GenericAssociationList) Position() (r token.Position) {
	if n == nil {
		return r
	}

	return n.GenericAssociation.Position()
}

// GenericSelection represents data reduced by production:
//
//	GenericSelection:
//	        "_Generic" '(' AssignmentExpression ',' GenericAssociationList ')'
type GenericSelection struct {
	assoc *GenericAssociation
	typer
	AssignmentExpression   ExpressionNode
	GenericAssociationList *GenericAssociationList
	Token                  Token
	Token2                 Token
	Token3                 Token
	Token4                 Token
}

// String implements fmt.Stringer.
func (n *GenericSelection) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *GenericSelection) Position() (r token.Position) {
	if n == nil {
		return r
	}

	if p := n.Token.Position(); p.IsValid() {
		return p
	}

	if p := n.Token2.Position(); p.IsValid() {
		return p
	}

	if p := n.AssignmentExpression.Position(); p.IsValid() {
		return p
	}

	if p := n.Token3.Position(); p.IsValid() {
		return p
	}

	if p := n.GenericAssociationList.Position(); p.IsValid() {
		return p
	}

	return n.Token4.Position()
}

// Associated returns the selected association of n, if any.
func (n *GenericSelection) Associated() *GenericAssociation { return n.assoc }

// IdentifierList represents data reduced by productions:
//
//	IdentifierList:
//	        IDENTIFIER
//	|       IdentifierList ',' IDENTIFIER
type IdentifierList struct {
	parameters     []*Parameter
	IdentifierList *IdentifierList
	Token          Token
	Token2         Token
}

// String implements fmt.Stringer.
func (n *IdentifierList) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *IdentifierList) Position() (r token.Position) {
	if n == nil {
		return r
	}

	return n.Token.Position()
}

// InitDeclaratorCase represents case numbers of production InitDeclarator
type InitDeclaratorCase int

// Values of type InitDeclaratorCase
const (
	InitDeclaratorDecl InitDeclaratorCase = iota
	InitDeclaratorInit
)

// String implements fmt.Stringer
func (n InitDeclaratorCase) String() string {
	switch n {
	case InitDeclaratorDecl:
		return "InitDeclaratorDecl"
	case InitDeclaratorInit:
		return "InitDeclaratorInit"
	default:
		return fmt.Sprintf("InitDeclaratorCase(%v)", int(n))
	}
}

// InitDeclarator represents data reduced by productions:
//
//	InitDeclarator:
//	        Declarator Asm                  // Case InitDeclaratorDecl
//	|       Declarator Asm '=' Initializer  // Case InitDeclaratorInit
type InitDeclarator struct {
	AttributeSpecifierList *AttributeSpecifierList
	Asm                    *Asm
	Case                   InitDeclaratorCase `PrettyPrint:"stringer,zero"`
	Declarator             *Declarator
	Initializer            *Initializer
	Token                  Token
}

// String implements fmt.Stringer.
func (n *InitDeclarator) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *InitDeclarator) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 0:
		if p := n.Declarator.Position(); p.IsValid() {
			return p
		}

		return n.Asm.Position()
	case 1:
		if p := n.Declarator.Position(); p.IsValid() {
			return p
		}

		if p := n.Asm.Position(); p.IsValid() {
			return p
		}

		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		return n.Initializer.Position()
	default:
		panic("internal error")
	}
}

// InitDeclaratorList represents data reduced by productions:
//
//	InitDeclaratorList:
//	        InitDeclarator
//	|       InitDeclaratorList ',' InitDeclarator
type InitDeclaratorList struct {
	AttributeSpecifierList *AttributeSpecifierList
	InitDeclarator         *InitDeclarator
	InitDeclaratorList     *InitDeclaratorList
	Token                  Token
}

// String implements fmt.Stringer.
func (n *InitDeclaratorList) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *InitDeclaratorList) Position() (r token.Position) {
	if n == nil {
		return r
	}

	return n.InitDeclarator.Position()
}

// InitializerCase represents case numbers of production Initializer
type InitializerCase int

// Values of type InitializerCase
const (
	InitializerExpr InitializerCase = iota
	InitializerInitList
)

// String implements fmt.Stringer
func (n InitializerCase) String() string {
	switch n {
	case InitializerExpr:
		return "InitializerExpr"
	case InitializerInitList:
		return "InitializerInitList"
	default:
		return fmt.Sprintf("InitializerCase(%v)", int(n))
	}
}

// Initializer represents data reduced by productions:
//
//	Initializer:
//	        AssignmentExpression         // Case InitializerExpr
//	|       '{' InitializerList ',' '}'  // Case InitializerInitList
type Initializer struct {
	nelems int64
	off    int64
	typer
	valuer
	AssignmentExpression ExpressionNode
	Case                 InitializerCase `PrettyPrint:"stringer,zero"`
	InitializerList      *InitializerList
	Token                Token
	Token2               Token
	Token3               Token
}

// String implements fmt.Stringer.
func (n *Initializer) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *Initializer) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 0:
		return n.AssignmentExpression.Position()
	case 1:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.InitializerList.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		return n.Token3.Position()
	default:
		panic("internal error")
	}
}

// Offset returns the offset of n within it's containing type.
func (n *Initializer) Offset() int64 { return n.off }

// Len returns the number of array elements initialized. It's normally one, but
// can be more using the [lo ... hi] designator.
func (n *Initializer) Len() int64 { return n.nelems }

// InitializerList represents data reduced by productions:
//
//	InitializerList:
//	        Designation Initializer
//	|       InitializerList ',' Designation Initializer
type InitializerList struct {
	Designation     *Designation
	Initializer     *Initializer
	InitializerList *InitializerList
	Token           Token
}

// String implements fmt.Stringer.
func (n *InitializerList) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *InitializerList) Position() (r token.Position) {
	if n == nil {
		return r
	}

	if p := n.Designation.Position(); p.IsValid() {
		return p
	}

	return n.Initializer.Position()
}

// IterationStatementCase represents case numbers of production IterationStatement
type IterationStatementCase int

// Values of type IterationStatementCase
const (
	IterationStatementWhile IterationStatementCase = iota
	IterationStatementDo
	IterationStatementFor
	IterationStatementForDecl
)

// String implements fmt.Stringer
func (n IterationStatementCase) String() string {
	switch n {
	case IterationStatementWhile:
		return "IterationStatementWhile"
	case IterationStatementDo:
		return "IterationStatementDo"
	case IterationStatementFor:
		return "IterationStatementFor"
	case IterationStatementForDecl:
		return "IterationStatementForDecl"
	default:
		return fmt.Sprintf("IterationStatementCase(%v)", int(n))
	}
}

// IterationStatement represents data reduced by productions:
//
//	IterationStatement:
//	        "while" '(' ExpressionList ')' Statement                                      // Case IterationStatementWhile
//	|       "do" Statement "while" '(' ExpressionList ')' ';'                             // Case IterationStatementDo
//	|       "for" '(' ExpressionList ';' ExpressionList ';' ExpressionList ')' Statement  // Case IterationStatementFor
//	|       "for" '(' Declaration ExpressionList ';' ExpressionList ')' Statement         // Case IterationStatementForDecl
type IterationStatement struct {
	Case            IterationStatementCase `PrettyPrint:"stringer,zero"`
	Declaration     Declaration
	ExpressionList  ExpressionNode
	ExpressionList2 ExpressionNode
	ExpressionList3 ExpressionNode
	Statement       Statement
	Token           Token
	Token2          Token
	Token3          Token
	Token4          Token
	Token5          Token
}

// String implements fmt.Stringer.
func (n *IterationStatement) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *IterationStatement) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 1:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Statement.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.Token3.Position(); p.IsValid() {
			return p
		}

		if p := n.ExpressionList.Position(); p.IsValid() {
			return p
		}

		if p := n.Token4.Position(); p.IsValid() {
			return p
		}

		return n.Token5.Position()
	case 3:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.Declaration.Position(); p.IsValid() {
			return p
		}

		if p := n.ExpressionList.Position(); p.IsValid() {
			return p
		}

		if p := n.Token3.Position(); p.IsValid() {
			return p
		}

		if p := n.ExpressionList2.Position(); p.IsValid() {
			return p
		}

		if p := n.Token4.Position(); p.IsValid() {
			return p
		}

		return n.Statement.Position()
	case 2:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.ExpressionList.Position(); p.IsValid() {
			return p
		}

		if p := n.Token3.Position(); p.IsValid() {
			return p
		}

		if p := n.ExpressionList2.Position(); p.IsValid() {
			return p
		}

		if p := n.Token4.Position(); p.IsValid() {
			return p
		}

		if p := n.ExpressionList3.Position(); p.IsValid() {
			return p
		}

		if p := n.Token5.Position(); p.IsValid() {
			return p
		}

		return n.Statement.Position()
	case 0:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.ExpressionList.Position(); p.IsValid() {
			return p
		}

		if p := n.Token3.Position(); p.IsValid() {
			return p
		}

		return n.Statement.Position()
	default:
		panic("internal error")
	}
}

// JumpStatementCase represents case numbers of production JumpStatement
type JumpStatementCase int

// Values of type JumpStatementCase
const (
	JumpStatementGoto JumpStatementCase = iota
	JumpStatementGotoExpr
	JumpStatementContinue
	JumpStatementBreak
	JumpStatementReturn
)

// String implements fmt.Stringer
func (n JumpStatementCase) String() string {
	switch n {
	case JumpStatementGoto:
		return "JumpStatementGoto"
	case JumpStatementGotoExpr:
		return "JumpStatementGotoExpr"
	case JumpStatementContinue:
		return "JumpStatementContinue"
	case JumpStatementBreak:
		return "JumpStatementBreak"
	case JumpStatementReturn:
		return "JumpStatementReturn"
	default:
		return fmt.Sprintf("JumpStatementCase(%v)", int(n))
	}
}

// JumpStatement represents data reduced by productions:
//
//	JumpStatement:
//	        "goto" IDENTIFIER ';'          // Case JumpStatementGoto
//	|       "goto" '*' ExpressionList ';'  // Case JumpStatementGotoExpr
//	|       "continue" ';'                 // Case JumpStatementContinue
//	|       "break" ';'                    // Case JumpStatementBreak
//	|       "return" ExpressionList ';'    // Case JumpStatementReturn
type JumpStatement struct {
	Case           JumpStatementCase `PrettyPrint:"stringer,zero"`
	ExpressionList ExpressionNode
	Token          Token
	Token2         Token
	Token3         Token
}

// String implements fmt.Stringer.
func (n *JumpStatement) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *JumpStatement) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 4:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.ExpressionList.Position(); p.IsValid() {
			return p
		}

		return n.Token2.Position()
	case 2, 3:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		return n.Token2.Position()
	case 1:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.ExpressionList.Position(); p.IsValid() {
			return p
		}

		return n.Token3.Position()
	case 0:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		return n.Token3.Position()
	default:
		panic("internal error")
	}
}

// LabelDeclaration represents data reduced by production:
//
//	LabelDeclaration:
//	        "__label__" IdentifierList ';'
type LabelDeclaration struct {
	IdentifierList *IdentifierList
	Token          Token
	Token2         Token
}

// String implements fmt.Stringer.
func (n *LabelDeclaration) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *LabelDeclaration) Position() (r token.Position) {
	if n == nil {
		return r
	}

	if p := n.Token.Position(); p.IsValid() {
		return p
	}

	if p := n.IdentifierList.Position(); p.IsValid() {
		return p
	}

	return n.Token2.Position()
}

// LabeledStatementCase represents case numbers of production LabeledStatement
type LabeledStatementCase int

// Values of type LabeledStatementCase
const (
	LabeledStatementLabel LabeledStatementCase = iota
	LabeledStatementCaseLabel
	LabeledStatementRange
	LabeledStatementDefault
)

// String implements fmt.Stringer
func (n LabeledStatementCase) String() string {
	switch n {
	case LabeledStatementLabel:
		return "LabeledStatementLabel"
	case LabeledStatementCaseLabel:
		return "LabeledStatementCaseLabel"
	case LabeledStatementRange:
		return "LabeledStatementRange"
	case LabeledStatementDefault:
		return "LabeledStatementDefault"
	default:
		return fmt.Sprintf("LabeledStatementCase(%v)", int(n))
	}
}

// LabeledStatement represents data reduced by productions:
//
//	LabeledStatement:
//	        IDENTIFIER ':' Statement                                          // Case LabeledStatementLabel
//	|       "case" ConstantExpression ':' Statement                           // Case LabeledStatementCaseLabel
//	|       "case" ConstantExpression "..." ConstantExpression ':' Statement  // Case LabeledStatementRange
//	|       "default" ':' Statement                                           // Case LabeledStatementDefault
type LabeledStatement struct {
	caseOrdinal         int
	Case                LabeledStatementCase `PrettyPrint:"stringer,zero"`
	ConstantExpression  ExpressionNode
	ConstantExpression2 ExpressionNode
	Statement           Statement
	Token               Token
	Token2              Token
	Token3              Token
}

// String implements fmt.Stringer.
func (n *LabeledStatement) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *LabeledStatement) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 2:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.ConstantExpression.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.ConstantExpression2.Position(); p.IsValid() {
			return p
		}

		if p := n.Token3.Position(); p.IsValid() {
			return p
		}

		return n.Statement.Position()
	case 1:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.ConstantExpression.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		return n.Statement.Position()
	case 0, 3:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		return n.Statement.Position()
	default:
		panic("internal error")
	}
}

// CaseOrdinal returns the zero based ordinal number of a labeled statement
// withing a switch statement.  Valid only for Case LabeledStatementCaseLabel
// and LabeledStatementDefault.
func (n *LabeledStatement) CaseOrdinal() int { return n.caseOrdinal }

// ParameterDeclarationCase represents case numbers of production ParameterDeclaration
type ParameterDeclarationCase int

// Values of type ParameterDeclarationCase
const (
	ParameterDeclarationDecl ParameterDeclarationCase = iota
	ParameterDeclarationAbstract
)

// String implements fmt.Stringer
func (n ParameterDeclarationCase) String() string {
	switch n {
	case ParameterDeclarationDecl:
		return "ParameterDeclarationDecl"
	case ParameterDeclarationAbstract:
		return "ParameterDeclarationAbstract"
	default:
		return fmt.Sprintf("ParameterDeclarationCase(%v)", int(n))
	}
}

// ParameterDeclaration represents data reduced by productions:
//
//	ParameterDeclaration:
//	        DeclarationSpecifiers Declarator          // Case ParameterDeclarationDecl
//	|       DeclarationSpecifiers AbstractDeclarator  // Case ParameterDeclarationAbstract
type ParameterDeclaration struct {
	AttributeSpecifierList *AttributeSpecifierList
	typer
	AbstractDeclarator    *AbstractDeclarator
	Case                  ParameterDeclarationCase `PrettyPrint:"stringer,zero"`
	DeclarationSpecifiers DeclarationSpecifiers
	Declarator            *Declarator
}

// String implements fmt.Stringer.
func (n *ParameterDeclaration) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *ParameterDeclaration) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 1:
		for _, s := range n.DeclarationSpecifiers {
			if p := s.Position(); p.IsValid() {
				return p
			}
		}

		return n.AbstractDeclarator.Position()
	case 0:
		for _, s := range n.DeclarationSpecifiers {
			if p := s.Position(); p.IsValid() {
				return p
			}
		}

		return n.Declarator.Position()
	default:
		panic("internal error")
	}
}

// ParameterList represents data reduced by productions:
//
//	ParameterList:
//	        ParameterDeclaration
//	|       ParameterList ',' ParameterDeclaration
type ParameterList struct {
	ParameterDeclaration *ParameterDeclaration
	ParameterList        *ParameterList
	Token                Token
}

// String implements fmt.Stringer.
func (n *ParameterList) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *ParameterList) Position() (r token.Position) {
	if n == nil {
		return r
	}

	return n.ParameterDeclaration.Position()
}

// ParameterTypeListCase represents case numbers of production ParameterTypeList
type ParameterTypeListCase int

// Values of type ParameterTypeListCase
const (
	ParameterTypeListList ParameterTypeListCase = iota
	ParameterTypeListVar
)

// String implements fmt.Stringer
func (n ParameterTypeListCase) String() string {
	switch n {
	case ParameterTypeListList:
		return "ParameterTypeListList"
	case ParameterTypeListVar:
		return "ParameterTypeListVar"
	default:
		return fmt.Sprintf("ParameterTypeListCase(%v)", int(n))
	}
}

// ParameterTypeList represents data reduced by productions:
//
//	ParameterTypeList:
//	        ParameterList            // Case ParameterTypeListList
//	|       ParameterList ',' "..."  // Case ParameterTypeListVar
type ParameterTypeList struct {
	Case          ParameterTypeListCase `PrettyPrint:"stringer,zero"`
	ParameterList *ParameterList
	Token         Token
	Token2        Token
}

// String implements fmt.Stringer.
func (n *ParameterTypeList) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *ParameterTypeList) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 0:
		return n.ParameterList.Position()
	case 1:
		if p := n.ParameterList.Position(); p.IsValid() {
			return p
		}

		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		return n.Token2.Position()
	default:
		panic("internal error")
	}
}

// PointerCase represents case numbers of production Pointer
type PointerCase int

// Values of type PointerCase
const (
	PointerTypeQual PointerCase = iota
	PointerPtr
	PointerBlock
)

// String implements fmt.Stringer
func (n PointerCase) String() string {
	switch n {
	case PointerTypeQual:
		return "PointerTypeQual"
	case PointerPtr:
		return "PointerPtr"
	case PointerBlock:
		return "PointerBlock"
	default:
		return fmt.Sprintf("PointerCase(%v)", int(n))
	}
}

// Pointer represents data reduced by productions:
//
//	Pointer:
//	        '*' TypeQualifiers          // Case PointerTypeQual
//	|       '*' TypeQualifiers Pointer  // Case PointerPtr
//	|       '^' TypeQualifiers          // Case PointerBlock
type Pointer struct {
	Case           PointerCase `PrettyPrint:"stringer,zero"`
	Pointer        *Pointer
	Token          Token
	TypeQualifiers *TypeQualifiers
}

// String implements fmt.Stringer.
func (n *Pointer) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *Pointer) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 0, 2:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		return n.TypeQualifiers.Position()
	case 1:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.TypeQualifiers.Position(); p.IsValid() {
			return p
		}

		return n.Pointer.Position()
	default:
		panic("internal error")
	}
}

// PostfixExpressionCase represents case numbers of production PostfixExpression
type PostfixExpressionCase int

// Values of type PostfixExpressionCase
const (
	PostfixExpressionPrimary PostfixExpressionCase = iota
	PostfixExpressionIndex
	PostfixExpressionCall
	PostfixExpressionSelect
	PostfixExpressionPSelect
	PostfixExpressionInc
	PostfixExpressionDec
	PostfixExpressionComplit
)

// String implements fmt.Stringer
func (n PostfixExpressionCase) String() string {
	switch n {
	case PostfixExpressionPrimary:
		return "PostfixExpressionPrimary"
	case PostfixExpressionIndex:
		return "PostfixExpressionIndex"
	case PostfixExpressionCall:
		return "PostfixExpressionCall"
	case PostfixExpressionSelect:
		return "PostfixExpressionSelect"
	case PostfixExpressionPSelect:
		return "PostfixExpressionPSelect"
	case PostfixExpressionInc:
		return "PostfixExpressionInc"
	case PostfixExpressionDec:
		return "PostfixExpressionDec"
	case PostfixExpressionComplit:
		return "PostfixExpressionComplit"
	default:
		return fmt.Sprintf("PostfixExpressionCase(%v)", int(n))
	}
}

// PostfixExpression represents data reduced by productions:
//
//	PostfixExpression:
//	        PrimaryExpression                                 // Case PostfixExpressionPrimary
//	|       PostfixExpression '[' ExpressionList ']'          // Case PostfixExpressionIndex
//	|       PostfixExpression '(' ArgumentExpressionList ')'  // Case PostfixExpressionCall
//	|       PostfixExpression '.' IDENTIFIER                  // Case PostfixExpressionSelect
//	|       PostfixExpression "->" IDENTIFIER                 // Case PostfixExpressionPSelect
//	|       PostfixExpression "++"                            // Case PostfixExpressionInc
//	|       PostfixExpression "--"                            // Case PostfixExpressionDec
//	|       '(' TypeName ')' '{' InitializerList ',' '}'      // Case PostfixExpressionComplit
type PostfixExpression struct {
	typer
	valuer
	field                  *Field
	ArgumentExpressionList *ArgumentExpressionList
	Case                   PostfixExpressionCase `PrettyPrint:"stringer,zero"`
	ExpressionList         ExpressionNode
	InitializerList        *InitializerList
	PostfixExpression      ExpressionNode
	PrimaryExpression      ExpressionNode
	Token                  Token
	Token2                 Token
	Token3                 Token
	Token4                 Token
	Token5                 Token
	TypeName               *TypeName
}

// String implements fmt.Stringer.
func (n *PostfixExpression) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *PostfixExpression) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 5, 6:
		if p := n.PostfixExpression.Position(); p.IsValid() {
			return p
		}

		return n.Token.Position()
	case 2:
		if p := n.PostfixExpression.Position(); p.IsValid() {
			return p
		}

		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.ArgumentExpressionList.Position(); p.IsValid() {
			return p
		}

		return n.Token2.Position()
	case 1:
		if p := n.PostfixExpression.Position(); p.IsValid() {
			return p
		}

		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.ExpressionList.Position(); p.IsValid() {
			return p
		}

		return n.Token2.Position()
	case 3, 4:
		if p := n.PostfixExpression.Position(); p.IsValid() {
			return p
		}

		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		return n.Token2.Position()
	case 0:
		return n.PrimaryExpression.Position()
	case 7:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.TypeName.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.Token3.Position(); p.IsValid() {
			return p
		}

		if p := n.InitializerList.Position(); p.IsValid() {
			return p
		}

		if p := n.Token4.Position(); p.IsValid() {
			return p
		}

		return n.Token5.Position()
	default:
		panic("internal error")
	}
}

// Field reports the resolved field for cases PostfixExpressionSelect and
// PostfixExpressionPSelect.
func (n *PostfixExpression) Field() *Field { return n.field }

// PrimaryExpressionCase represents case numbers of production PrimaryExpression
type PrimaryExpressionCase int

// Values of type PrimaryExpressionCase
const (
	PrimaryExpressionIdent PrimaryExpressionCase = iota
	PrimaryExpressionInt
	PrimaryExpressionFloat
	PrimaryExpressionChar
	PrimaryExpressionLChar
	PrimaryExpressionString
	PrimaryExpressionLString
	PrimaryExpressionExpr
	PrimaryExpressionStmt
	PrimaryExpressionGeneric
)

// String implements fmt.Stringer
func (n PrimaryExpressionCase) String() string {
	switch n {
	case PrimaryExpressionIdent:
		return "PrimaryExpressionIdent"
	case PrimaryExpressionInt:
		return "PrimaryExpressionInt"
	case PrimaryExpressionFloat:
		return "PrimaryExpressionFloat"
	case PrimaryExpressionChar:
		return "PrimaryExpressionChar"
	case PrimaryExpressionLChar:
		return "PrimaryExpressionLChar"
	case PrimaryExpressionString:
		return "PrimaryExpressionString"
	case PrimaryExpressionLString:
		return "PrimaryExpressionLString"
	case PrimaryExpressionExpr:
		return "PrimaryExpressionExpr"
	case PrimaryExpressionStmt:
		return "PrimaryExpressionStmt"
	case PrimaryExpressionGeneric:
		return "PrimaryExpressionGeneric"
	default:
		return fmt.Sprintf("PrimaryExpressionCase(%v)", int(n))
	}
}

// PrimaryExpression represents data reduced by productions:
//
//	PrimaryExpression:
//	        IDENTIFIER                 // Case PrimaryExpressionIdent
//	|       INTCONST                   // Case PrimaryExpressionInt
//	|       FLOATCONST                 // Case PrimaryExpressionFloat
//	|       CHARCONST                  // Case PrimaryExpressionChar
//	|       LONGCHARCONST              // Case PrimaryExpressionLChar
//	|       STRINGLITERAL              // Case PrimaryExpressionString
//	|       LONGSTRINGLITERAL          // Case PrimaryExpressionLString
//	|       '(' ExpressionList ')'     // Case PrimaryExpressionExpr
//	|       '(' CompoundStatement ')'  // Case PrimaryExpressionStmt
//	|       GenericSelection           // Case PrimaryExpressionGeneric
type PrimaryExpression struct {
	m *Macro
	lexicalScoper
	resolvedTo Node
	typer
	valuer
	Case              PrimaryExpressionCase `PrettyPrint:"stringer,zero"`
	CompoundStatement *CompoundStatement
	ExpressionList    ExpressionNode
	GenericSelection  *GenericSelection
	Token             Token
	Token2            Token
}

// String implements fmt.Stringer.
func (n *PrimaryExpression) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *PrimaryExpression) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 9:
		return n.GenericSelection.Position()
	case 0, 1, 2, 3, 4, 5, 6:
		return n.Token.Position()
	case 8:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.CompoundStatement.Position(); p.IsValid() {
			return p
		}

		return n.Token2.Position()
	case 7:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.ExpressionList.Position(); p.IsValid() {
			return p
		}

		return n.Token2.Position()
	default:
		panic("internal error")
	}
}

// ResolvedTo returns the node n resolved to when n.Case is
// PrimaryExpressionIdent.
func (n *PrimaryExpression) ResolvedTo() Node { return n.resolvedTo }

// Macro returns the single token, object-like, constant macro that produced
// this node, if any.
func (n *PrimaryExpression) Macro() *Macro { return n.m }

// SelectionStatementCase represents case numbers of production SelectionStatement
type SelectionStatementCase int

// Values of type SelectionStatementCase
const (
	SelectionStatementIf SelectionStatementCase = iota
	SelectionStatementIfElse
	SelectionStatementSwitch
)

// String implements fmt.Stringer
func (n SelectionStatementCase) String() string {
	switch n {
	case SelectionStatementIf:
		return "SelectionStatementIf"
	case SelectionStatementIfElse:
		return "SelectionStatementIfElse"
	case SelectionStatementSwitch:
		return "SelectionStatementSwitch"
	default:
		return fmt.Sprintf("SelectionStatementCase(%v)", int(n))
	}
}

// SelectionStatement represents data reduced by productions:
//
//	SelectionStatement:
//	        "if" '(' ExpressionList ')' Statement                   // Case SelectionStatementIf
//	|       "if" '(' ExpressionList ')' Statement "else" Statement  // Case SelectionStatementIfElse
//	|       "switch" '(' ExpressionList ')' Statement               // Case SelectionStatementSwitch
type SelectionStatement struct {
	switchCases    int
	Case           SelectionStatementCase `PrettyPrint:"stringer,zero"`
	ExpressionList ExpressionNode
	Statement      Statement
	Statement2     Statement
	Token          Token
	Token2         Token
	Token3         Token
	Token4         Token
}

// String implements fmt.Stringer.
func (n *SelectionStatement) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *SelectionStatement) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 0, 2:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.ExpressionList.Position(); p.IsValid() {
			return p
		}

		if p := n.Token3.Position(); p.IsValid() {
			return p
		}

		return n.Statement.Position()
	case 1:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.ExpressionList.Position(); p.IsValid() {
			return p
		}

		if p := n.Token3.Position(); p.IsValid() {
			return p
		}

		if p := n.Statement.Position(); p.IsValid() {
			return p
		}

		if p := n.Token4.Position(); p.IsValid() {
			return p
		}

		return n.Statement2.Position()
	default:
		panic("internal error")
	}
}

// Cases returns the combined number of "case" and "default" labels in a switch
// statement. Valid for Case == SelectionStatementSwitch.
func (n *SelectionStatement) Cases() int { return n.switchCases }

// SpecifierQualifierListCase represents case numbers of production SpecifierQualifierList
type SpecifierQualifierListCase int

// Values of type SpecifierQualifierListCase
const (
	SpecifierQualifierListTypeSpec SpecifierQualifierListCase = iota
	SpecifierQualifierListTypeQual
	SpecifierQualifierListAlignSpec
)

// String implements fmt.Stringer
func (n SpecifierQualifierListCase) String() string {
	switch n {
	case SpecifierQualifierListTypeSpec:
		return "SpecifierQualifierListTypeSpec"
	case SpecifierQualifierListTypeQual:
		return "SpecifierQualifierListTypeQual"
	case SpecifierQualifierListAlignSpec:
		return "SpecifierQualifierListAlignSpec"
	default:
		return fmt.Sprintf("SpecifierQualifierListCase(%v)", int(n))
	}
}

// SpecifierQualifierList represents data reduced by productions:
//
//	SpecifierQualifierList:
//	        TypeSpecifier SpecifierQualifierList       // Case SpecifierQualifierListTypeSpec
//	|       TypeQualifier SpecifierQualifierList       // Case SpecifierQualifierListTypeQual
//	|       AlignmentSpecifier SpecifierQualifierList  // Case SpecifierQualifierListAlignSpec
type SpecifierQualifierList struct {
	AttributeSpecifierList *AttributeSpecifierList
	AlignmentSpecifier     *AlignmentSpecifier
	Case                   SpecifierQualifierListCase `PrettyPrint:"stringer,zero"`
	SpecifierQualifierList *SpecifierQualifierList
	TypeQualifier          *TypeQualifier
	TypeSpecifier          *TypeSpecifier
}

// String implements fmt.Stringer.
func (n *SpecifierQualifierList) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *SpecifierQualifierList) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 2:
		if p := n.AlignmentSpecifier.Position(); p.IsValid() {
			return p
		}

		return n.SpecifierQualifierList.Position()
	case 1:
		if p := n.TypeQualifier.Position(); p.IsValid() {
			return p
		}

		return n.SpecifierQualifierList.Position()
	case 0:
		if p := n.TypeSpecifier.Position(); p.IsValid() {
			return p
		}

		return n.SpecifierQualifierList.Position()
	default:
		panic("internal error")
	}
}

// Statement represents data reduced by productions:
//
//	Statement:
//	        LabeledStatement     // Case StatementLabeled
//	|       CompoundStatement    // Case StatementCompound
//	|       ExpressionStatement  // Case StatementExpr
//	|       SelectionStatement   // Case StatementSelection
//	|       IterationStatement   // Case StatementIteration
//	|       JumpStatement        // Case StatementJump
//	|       AsmStatement         // Case StatementAsm
type Statement interface {
	Node
	fmt.Stringer
	isStatement()
	BlockItem
}

func (*LabeledStatement) isStatement()    {}
func (*CompoundStatement) isStatement()   {}
func (*ExpressionStatement) isStatement() {}
func (*SelectionStatement) isStatement()  {}
func (*IterationStatement) isStatement()  {}
func (*JumpStatement) isStatement()       {}
func (*AsmStatement) isStatement()        {}

// StaticAssertDeclaration represents data reduced by production:
//
//	StaticAssertDeclaration:
//	        "_Static_assert" '(' ConstantExpression ',' STRINGLITERAL ')'
type StaticAssertDeclaration struct {
	ConstantExpression ExpressionNode
	Token              Token
	Token2             Token
	Token3             Token
	Token4             Token
	Token5             Token
}

// String implements fmt.Stringer.
func (n *StaticAssertDeclaration) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *StaticAssertDeclaration) Position() (r token.Position) {
	if n == nil {
		return r
	}

	if p := n.Token.Position(); p.IsValid() {
		return p
	}

	if p := n.Token2.Position(); p.IsValid() {
		return p
	}

	if p := n.ConstantExpression.Position(); p.IsValid() {
		return p
	}

	if p := n.Token3.Position(); p.IsValid() {
		return p
	}

	if p := n.Token4.Position(); p.IsValid() {
		return p
	}

	return n.Token5.Position()
}

// StorageClassSpecifierCase represents case numbers of production StorageClassSpecifier
type StorageClassSpecifierCase int

// Values of type StorageClassSpecifierCase
const (
	StorageClassSpecifierTypedef StorageClassSpecifierCase = iota
	StorageClassSpecifierExtern
	StorageClassSpecifierStatic
	StorageClassSpecifierAuto
	StorageClassSpecifierRegister
	StorageClassSpecifierThreadLocal
	StorageClassSpecifierDeclspec
)

// String implements fmt.Stringer
func (n StorageClassSpecifierCase) String() string {
	switch n {
	case StorageClassSpecifierTypedef:
		return "StorageClassSpecifierTypedef"
	case StorageClassSpecifierExtern:
		return "StorageClassSpecifierExtern"
	case StorageClassSpecifierStatic:
		return "StorageClassSpecifierStatic"
	case StorageClassSpecifierAuto:
		return "StorageClassSpecifierAuto"
	case StorageClassSpecifierRegister:
		return "StorageClassSpecifierRegister"
	case StorageClassSpecifierThreadLocal:
		return "StorageClassSpecifierThreadLocal"
	case StorageClassSpecifierDeclspec:
		return "StorageClassSpecifierDeclspec"
	default:
		return fmt.Sprintf("StorageClassSpecifierCase(%v)", int(n))
	}
}

// StorageClassSpecifier represents data reduced by productions:
//
//	StorageClassSpecifier:
//	        "typedef"             // Case StorageClassSpecifierTypedef
//	|       "extern"              // Case StorageClassSpecifierExtern
//	|       "static"              // Case StorageClassSpecifierStatic
//	|       "auto"                // Case StorageClassSpecifierAuto
//	|       "register"            // Case StorageClassSpecifierRegister
//	|       "_Thread_local"       // Case StorageClassSpecifierThreadLocal
//	|       "__declspec" '(' ')'  // Case StorageClassSpecifierDeclspec
type StorageClassSpecifier struct {
	Declspecs []Token
	Case      StorageClassSpecifierCase `PrettyPrint:"stringer,zero"`
	Token     Token
	Token2    Token
	Token3    Token
}

// String implements fmt.Stringer.
func (n *StorageClassSpecifier) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *StorageClassSpecifier) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 0, 1, 2, 3, 4, 5:
		return n.Token.Position()
	case 6:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		return n.Token3.Position()
	default:
		panic("internal error")
	}
}

// StructDeclarationCase represents case numbers of production StructDeclaration
type StructDeclarationCase int

// Values of type StructDeclarationCase
const (
	StructDeclarationDecl StructDeclarationCase = iota
	StructDeclarationAssert
)

// String implements fmt.Stringer
func (n StructDeclarationCase) String() string {
	switch n {
	case StructDeclarationDecl:
		return "StructDeclarationDecl"
	case StructDeclarationAssert:
		return "StructDeclarationAssert"
	default:
		return fmt.Sprintf("StructDeclarationCase(%v)", int(n))
	}
}

// StructDeclaration represents data reduced by productions:
//
//	StructDeclaration:
//	        SpecifierQualifierList StructDeclaratorList ';'  // Case StructDeclarationDecl
//	|       StaticAssertDeclaration                          // Case StructDeclarationAssert
type StructDeclaration struct {
	AttributeSpecifierList  *AttributeSpecifierList
	Case                    StructDeclarationCase `PrettyPrint:"stringer,zero"`
	SpecifierQualifierList  *SpecifierQualifierList
	StaticAssertDeclaration *StaticAssertDeclaration
	StructDeclaratorList    *StructDeclaratorList
	Token                   Token
}

// String implements fmt.Stringer.
func (n *StructDeclaration) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *StructDeclaration) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 0:
		if p := n.SpecifierQualifierList.Position(); p.IsValid() {
			return p
		}

		if p := n.StructDeclaratorList.Position(); p.IsValid() {
			return p
		}

		return n.Token.Position()
	case 1:
		return n.StaticAssertDeclaration.Position()
	default:
		panic("internal error")
	}
}

// StructDeclarationList represents data reduced by productions:
//
//	StructDeclarationList:
//	        StructDeclaration
//	|       StructDeclarationList StructDeclaration
type StructDeclarationList struct {
	StructDeclaration     *StructDeclaration
	StructDeclarationList *StructDeclarationList
}

// String implements fmt.Stringer.
func (n *StructDeclarationList) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *StructDeclarationList) Position() (r token.Position) {
	if n == nil {
		return r
	}

	return n.StructDeclaration.Position()
}

// StructDeclaratorCase represents case numbers of production StructDeclarator
type StructDeclaratorCase int

// Values of type StructDeclaratorCase
const (
	StructDeclaratorDecl StructDeclaratorCase = iota
	StructDeclaratorBitField
)

// String implements fmt.Stringer
func (n StructDeclaratorCase) String() string {
	switch n {
	case StructDeclaratorDecl:
		return "StructDeclaratorDecl"
	case StructDeclaratorBitField:
		return "StructDeclaratorBitField"
	default:
		return fmt.Sprintf("StructDeclaratorCase(%v)", int(n))
	}
}

// StructDeclarator represents data reduced by productions:
//
//	StructDeclarator:
//	        Declarator                         // Case StructDeclaratorDecl
//	|       Declarator ':' ConstantExpression  // Case StructDeclaratorBitField
type StructDeclarator struct {
	Case               StructDeclaratorCase `PrettyPrint:"stringer,zero"`
	ConstantExpression ExpressionNode
	Declarator         *Declarator
	Token              Token
}

// String implements fmt.Stringer.
func (n *StructDeclarator) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *StructDeclarator) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 0:
		return n.Declarator.Position()
	case 1:
		if p := n.Declarator.Position(); p.IsValid() {
			return p
		}

		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		return n.ConstantExpression.Position()
	default:
		panic("internal error")
	}
}

// StructDeclaratorList represents data reduced by productions:
//
//	StructDeclaratorList:
//	        StructDeclarator
//	|       StructDeclaratorList ',' StructDeclarator
type StructDeclaratorList struct {
	StructDeclarator     *StructDeclarator
	StructDeclaratorList *StructDeclaratorList
	Token                Token
}

// String implements fmt.Stringer.
func (n *StructDeclaratorList) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *StructDeclaratorList) Position() (r token.Position) {
	if n == nil {
		return r
	}

	return n.StructDeclarator.Position()
}

// StructOrUnionCase represents case numbers of production StructOrUnion
type StructOrUnionCase int

// Values of type StructOrUnionCase
const (
	StructOrUnionStruct StructOrUnionCase = iota
	StructOrUnionUnion
)

// String implements fmt.Stringer
func (n StructOrUnionCase) String() string {
	switch n {
	case StructOrUnionStruct:
		return "StructOrUnionStruct"
	case StructOrUnionUnion:
		return "StructOrUnionUnion"
	default:
		return fmt.Sprintf("StructOrUnionCase(%v)", int(n))
	}
}

// StructOrUnion represents data reduced by productions:
//
//	StructOrUnion:
//	        "struct"  // Case StructOrUnionStruct
//	|       "union"   // Case StructOrUnionUnion
type StructOrUnion struct {
	Case  StructOrUnionCase `PrettyPrint:"stringer,zero"`
	Token Token
}

// String implements fmt.Stringer.
func (n *StructOrUnion) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *StructOrUnion) Position() (r token.Position) {
	if n == nil {
		return r
	}

	return n.Token.Position()
}

// StructOrUnionSpecifierCase represents case numbers of production StructOrUnionSpecifier
type StructOrUnionSpecifierCase int

// Values of type StructOrUnionSpecifierCase
const (
	StructOrUnionSpecifierDef StructOrUnionSpecifierCase = iota
	StructOrUnionSpecifierTag
)

// String implements fmt.Stringer
func (n StructOrUnionSpecifierCase) String() string {
	switch n {
	case StructOrUnionSpecifierDef:
		return "StructOrUnionSpecifierDef"
	case StructOrUnionSpecifierTag:
		return "StructOrUnionSpecifierTag"
	default:
		return fmt.Sprintf("StructOrUnionSpecifierCase(%v)", int(n))
	}
}

// StructOrUnionSpecifier represents data reduced by productions:
//
//	StructOrUnionSpecifier:
//	        StructOrUnion IDENTIFIER '{' StructDeclarationList '}'  // Case StructOrUnionSpecifierDef
//	|       StructOrUnion IDENTIFIER                                // Case StructOrUnionSpecifierTag
type StructOrUnionSpecifier struct {
	AttributeSpecifierList  *AttributeSpecifierList
	AttributeSpecifierList2 *AttributeSpecifierList
	lexicalScoper
	visible
	typer
	Case                  StructOrUnionSpecifierCase `PrettyPrint:"stringer,zero"`
	StructDeclarationList *StructDeclarationList
	StructOrUnion         *StructOrUnion
	Token                 Token
	Token2                Token
	Token3                Token
}

// String implements fmt.Stringer.
func (n *StructOrUnionSpecifier) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *StructOrUnionSpecifier) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 1:
		if p := n.StructOrUnion.Position(); p.IsValid() {
			return p
		}

		return n.Token.Position()
	case 0:
		if p := n.StructOrUnion.Position(); p.IsValid() {
			return p
		}

		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.StructDeclarationList.Position(); p.IsValid() {
			return p
		}

		return n.Token3.Position()
	default:
		panic("internal error")
	}
}

// TypeName represents data reduced by production:
//
//	TypeName:
//	        SpecifierQualifierList AbstractDeclarator
type TypeName struct {
	typer
	AbstractDeclarator     *AbstractDeclarator
	SpecifierQualifierList *SpecifierQualifierList
}

// String implements fmt.Stringer.
func (n *TypeName) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *TypeName) Position() (r token.Position) {
	if n == nil {
		return r
	}

	if p := n.SpecifierQualifierList.Position(); p.IsValid() {
		return p
	}

	return n.AbstractDeclarator.Position()
}

// TypeQualifierCase represents case numbers of production TypeQualifier
type TypeQualifierCase int

// Values of type TypeQualifierCase
const (
	TypeQualifierConst TypeQualifierCase = iota
	TypeQualifierRestrict
	TypeQualifierVolatile
	TypeQualifierAtomic
	TypeQualifierNonnull
	TypeQualifierAttr
)

// String implements fmt.Stringer
func (n TypeQualifierCase) String() string {
	switch n {
	case TypeQualifierConst:
		return "TypeQualifierConst"
	case TypeQualifierRestrict:
		return "TypeQualifierRestrict"
	case TypeQualifierVolatile:
		return "TypeQualifierVolatile"
	case TypeQualifierAtomic:
		return "TypeQualifierAtomic"
	case TypeQualifierNonnull:
		return "TypeQualifierNonnull"
	case TypeQualifierAttr:
		return "TypeQualifierAttr"
	default:
		return fmt.Sprintf("TypeQualifierCase(%v)", int(n))
	}
}

// TypeQualifier represents data reduced by productions:
//
//	TypeQualifier:
//	        "const"          // Case TypeQualifierConst
//	|       "restrict"       // Case TypeQualifierRestrict
//	|       "volatile"       // Case TypeQualifierVolatile
//	|       "_Atomic"        // Case TypeQualifierAtomic
//	|       "_Nonnull"       // Case TypeQualifierNonnull
//	|       "__attribute__"  // Case TypeQualifierAttr
type TypeQualifier struct {
	AttributeSpecifierList *AttributeSpecifierList
	Case                   TypeQualifierCase `PrettyPrint:"stringer,zero"`
	Token                  Token
}

// String implements fmt.Stringer.
func (n *TypeQualifier) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *TypeQualifier) Position() (r token.Position) {
	if n == nil {
		return r
	}

	return n.Token.Position()
}

// TypeQualifiersCase represents case numbers of production TypeQualifiers
type TypeQualifiersCase int

// Values of type TypeQualifiersCase
const (
	TypeQualifiersTypeQual TypeQualifiersCase = iota
	_
)

// String implements fmt.Stringer
func (n TypeQualifiersCase) String() string {
	switch n {
	case TypeQualifiersTypeQual:
		return "TypeQualifiersTypeQual"
	default:
		return fmt.Sprintf("TypeQualifiersCase(%v)", int(n))
	}
}

// TypeQualifiers represents data reduced by productions:
//
//	TypeQualifiers:
//	        TypeQualifier                 // Case TypeQualifiersTypeQual
//	|       TypeQualifiers TypeQualifier  // Case 1
type TypeQualifiers struct {
	Case           TypeQualifiersCase `PrettyPrint:"stringer,zero"`
	TypeQualifier  *TypeQualifier
	TypeQualifiers *TypeQualifiers
}

// String implements fmt.Stringer.
func (n *TypeQualifiers) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *TypeQualifiers) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 0:
		return n.TypeQualifier.Position()
	case 1:
		if p := n.TypeQualifiers.Position(); p.IsValid() {
			return p
		}

		return n.TypeQualifier.Position()
	default:
		panic("internal error")
	}
}

// TypeSpecifierCase represents case numbers of production TypeSpecifier
type TypeSpecifierCase int

// Values of type TypeSpecifierCase
const (
	TypeSpecifierVoid TypeSpecifierCase = iota
	TypeSpecifierChar
	TypeSpecifierShort
	TypeSpecifierInt
	TypeSpecifierInt128
	TypeSpecifierUint128
	TypeSpecifierLong
	TypeSpecifierFloat
	TypeSpecifierFloat16
	TypeSpecifierDecimal32
	TypeSpecifierDecimal64
	TypeSpecifierDecimal128
	TypeSpecifierFloat128
	TypeSpecifierFloat128x
	TypeSpecifierDouble
	TypeSpecifierSigned
	TypeSpecifierUnsigned
	TypeSpecifierBool
	TypeSpecifierComplex
	TypeSpecifierImaginary
	TypeSpecifierStructOrUnion
	TypeSpecifierEnum
	TypeSpecifierTypeName
	TypeSpecifierTypeofExpr
	TypeSpecifierTypeofType
	TypeSpecifierAtomic
	TypeSpecifierFloat32
	TypeSpecifierFloat64
	TypeSpecifierFloat32x
	TypeSpecifierFloat64x
)

// String implements fmt.Stringer
func (n TypeSpecifierCase) String() string {
	switch n {
	case TypeSpecifierVoid:
		return "TypeSpecifierVoid"
	case TypeSpecifierChar:
		return "TypeSpecifierChar"
	case TypeSpecifierShort:
		return "TypeSpecifierShort"
	case TypeSpecifierInt:
		return "TypeSpecifierInt"
	case TypeSpecifierInt128:
		return "TypeSpecifierInt128"
	case TypeSpecifierUint128:
		return "TypeSpecifierUint128"
	case TypeSpecifierLong:
		return "TypeSpecifierLong"
	case TypeSpecifierFloat:
		return "TypeSpecifierFloat"
	case TypeSpecifierFloat16:
		return "TypeSpecifierFloat16"
	case TypeSpecifierDecimal32:
		return "TypeSpecifierDecimal32"
	case TypeSpecifierDecimal64:
		return "TypeSpecifierDecimal64"
	case TypeSpecifierDecimal128:
		return "TypeSpecifierDecimal128"
	case TypeSpecifierFloat128:
		return "TypeSpecifierFloat128"
	case TypeSpecifierFloat128x:
		return "TypeSpecifierFloat128x"
	case TypeSpecifierDouble:
		return "TypeSpecifierDouble"
	case TypeSpecifierSigned:
		return "TypeSpecifierSigned"
	case TypeSpecifierUnsigned:
		return "TypeSpecifierUnsigned"
	case TypeSpecifierBool:
		return "TypeSpecifierBool"
	case TypeSpecifierComplex:
		return "TypeSpecifierComplex"
	case TypeSpecifierImaginary:
		return "TypeSpecifierImaginary"
	case TypeSpecifierStructOrUnion:
		return "TypeSpecifierStructOrUnion"
	case TypeSpecifierEnum:
		return "TypeSpecifierEnum"
	case TypeSpecifierTypeName:
		return "TypeSpecifierTypeName"
	case TypeSpecifierTypeofExpr:
		return "TypeSpecifierTypeofExpr"
	case TypeSpecifierTypeofType:
		return "TypeSpecifierTypeofType"
	case TypeSpecifierAtomic:
		return "TypeSpecifierAtomic"
	case TypeSpecifierFloat32:
		return "TypeSpecifierFloat32"
	case TypeSpecifierFloat64:
		return "TypeSpecifierFloat64"
	case TypeSpecifierFloat32x:
		return "TypeSpecifierFloat32x"
	case TypeSpecifierFloat64x:
		return "TypeSpecifierFloat64x"
	default:
		return fmt.Sprintf("TypeSpecifierCase(%v)", int(n))
	}
}

// TypeSpecifier represents data reduced by productions:
//
//	TypeSpecifier:
//	        "void"                           // Case TypeSpecifierVoid
//	|       "char"                           // Case TypeSpecifierChar
//	|       "short"                          // Case TypeSpecifierShort
//	|       "int"                            // Case TypeSpecifierInt
//	|       "__int128"                       // Case TypeSpecifierInt128
//	|       "__uint128_t"                    // Case TypeSpecifierUint128
//	|       "long"                           // Case TypeSpecifierLong
//	|       "float"                          // Case TypeSpecifierFloat
//	|       "_Float16"                       // Case TypeSpecifierFloat16
//	|       "_Decimal32"                     // Case TypeSpecifierDecimal32
//	|       "_Decimal64"                     // Case TypeSpecifierDecimal64
//	|       "_Decimal128"                    // Case TypeSpecifierDecimal128
//	|       "_Float128"                      // Case TypeSpecifierFloat128
//	|       "_Float128x"                     // Case TypeSpecifierFloat128x
//	|       "double"                         // Case TypeSpecifierDouble
//	|       "signed"                         // Case TypeSpecifierSigned
//	|       "unsigned"                       // Case TypeSpecifierUnsigned
//	|       "_Bool"                          // Case TypeSpecifierBool
//	|       "_Complex"                       // Case TypeSpecifierComplex
//	|       "_Imaginary"                     // Case TypeSpecifierImaginary
//	|       StructOrUnionSpecifier           // Case TypeSpecifierStructOrUnion
//	|       EnumSpecifier                    // Case TypeSpecifierEnum
//	|       TYPENAME                         // Case TypeSpecifierTypeName
//	|       "typeof" '(' ExpressionList ')'  // Case TypeSpecifierTypeofExpr
//	|       "typeof" '(' TypeName ')'        // Case TypeSpecifierTypeofType
//	|       AtomicTypeSpecifier              // Case TypeSpecifierAtomic
//	|       "_Float32"                       // Case TypeSpecifierFloat32
//	|       "_Float64"                       // Case TypeSpecifierFloat64
//	|       "_Float32x"                      // Case TypeSpecifierFloat32x
//	|       "_Float64x"                      // Case TypeSpecifierFloat64x
type TypeSpecifier struct {
	lexicalScoper
	AtomicTypeSpecifier    *AtomicTypeSpecifier
	Case                   TypeSpecifierCase `PrettyPrint:"stringer,zero"`
	EnumSpecifier          *EnumSpecifier
	ExpressionList         ExpressionNode
	StructOrUnionSpecifier *StructOrUnionSpecifier
	Token                  Token
	Token2                 Token
	Token3                 Token
	TypeName               *TypeName
}

// String implements fmt.Stringer.
func (n *TypeSpecifier) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *TypeSpecifier) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 25:
		return n.AtomicTypeSpecifier.Position()
	case 21:
		return n.EnumSpecifier.Position()
	case 20:
		return n.StructOrUnionSpecifier.Position()
	case 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 22, 26, 27, 28, 29:
		return n.Token.Position()
	case 23:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.ExpressionList.Position(); p.IsValid() {
			return p
		}

		return n.Token3.Position()
	case 24:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.TypeName.Position(); p.IsValid() {
			return p
		}

		return n.Token3.Position()
	default:
		panic("internal error")
	}
}

// UnaryExpressionCase represents case numbers of production UnaryExpression
type UnaryExpressionCase int

// Values of type UnaryExpressionCase
const (
	UnaryExpressionPostfix UnaryExpressionCase = iota
	UnaryExpressionInc
	UnaryExpressionDec
	UnaryExpressionAddrof
	UnaryExpressionDeref
	UnaryExpressionPlus
	UnaryExpressionMinus
	UnaryExpressionCpl
	UnaryExpressionNot
	UnaryExpressionSizeofExpr
	UnaryExpressionSizeofType
	UnaryExpressionLabelAddr
	UnaryExpressionAlignofExpr
	UnaryExpressionAlignofType
	UnaryExpressionImag
	UnaryExpressionReal
)

// String implements fmt.Stringer
func (n UnaryExpressionCase) String() string {
	switch n {
	case UnaryExpressionPostfix:
		return "UnaryExpressionPostfix"
	case UnaryExpressionInc:
		return "UnaryExpressionInc"
	case UnaryExpressionDec:
		return "UnaryExpressionDec"
	case UnaryExpressionAddrof:
		return "UnaryExpressionAddrof"
	case UnaryExpressionDeref:
		return "UnaryExpressionDeref"
	case UnaryExpressionPlus:
		return "UnaryExpressionPlus"
	case UnaryExpressionMinus:
		return "UnaryExpressionMinus"
	case UnaryExpressionCpl:
		return "UnaryExpressionCpl"
	case UnaryExpressionNot:
		return "UnaryExpressionNot"
	case UnaryExpressionSizeofExpr:
		return "UnaryExpressionSizeofExpr"
	case UnaryExpressionSizeofType:
		return "UnaryExpressionSizeofType"
	case UnaryExpressionLabelAddr:
		return "UnaryExpressionLabelAddr"
	case UnaryExpressionAlignofExpr:
		return "UnaryExpressionAlignofExpr"
	case UnaryExpressionAlignofType:
		return "UnaryExpressionAlignofType"
	case UnaryExpressionImag:
		return "UnaryExpressionImag"
	case UnaryExpressionReal:
		return "UnaryExpressionReal"
	default:
		return fmt.Sprintf("UnaryExpressionCase(%v)", int(n))
	}
}

// UnaryExpression represents data reduced by productions:
//
//	UnaryExpression:
//	        PostfixExpression            // Case UnaryExpressionPostfix
//	|       "++" UnaryExpression         // Case UnaryExpressionInc
//	|       "--" UnaryExpression         // Case UnaryExpressionDec
//	|       '&' CastExpression           // Case UnaryExpressionAddrof
//	|       '*' CastExpression           // Case UnaryExpressionDeref
//	|       '+' CastExpression           // Case UnaryExpressionPlus
//	|       '-' CastExpression           // Case UnaryExpressionMinus
//	|       '~' CastExpression           // Case UnaryExpressionCpl
//	|       '!' CastExpression           // Case UnaryExpressionNot
//	|       "sizeof" UnaryExpression     // Case UnaryExpressionSizeofExpr
//	|       "sizeof" '(' TypeName ')'    // Case UnaryExpressionSizeofType
//	|       "&&" IDENTIFIER              // Case UnaryExpressionLabelAddr
//	|       "_Alignof" UnaryExpression   // Case UnaryExpressionAlignofExpr
//	|       "_Alignof" '(' TypeName ')'  // Case UnaryExpressionAlignofType
//	|       "__imag__" UnaryExpression   // Case UnaryExpressionImag
//	|       "__real__" UnaryExpression   // Case UnaryExpressionReal
type UnaryExpression struct {
	typer
	valuer
	Case              UnaryExpressionCase `PrettyPrint:"stringer,zero"`
	CastExpression    ExpressionNode
	PostfixExpression ExpressionNode
	Token             Token
	Token2            Token
	Token3            Token
	TypeName          *TypeName
	UnaryExpression   ExpressionNode
}

// String implements fmt.Stringer.
func (n *UnaryExpression) String() string { return PrettyString(n) }

// Position reports the position of the first component of n, if available.
func (n *UnaryExpression) Position() (r token.Position) {
	if n == nil {
		return r
	}

	switch n.Case {
	case 0:
		return n.PostfixExpression.Position()
	case 3, 4, 5, 6, 7, 8:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		return n.CastExpression.Position()
	case 11:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		return n.Token2.Position()
	case 10, 13:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		if p := n.Token2.Position(); p.IsValid() {
			return p
		}

		if p := n.TypeName.Position(); p.IsValid() {
			return p
		}

		return n.Token3.Position()
	case 1, 2, 9, 12, 14, 15:
		if p := n.Token.Position(); p.IsValid() {
			return p
		}

		return n.UnaryExpression.Position()
	default:
		panic("internal error")
	}
}
