// Copyright 2022 The CC Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package cc // import "modernc.org/cc/v5"

import (
	"math/big"
)

var (
	_ Value = (*ComplexLongDoubleValue)(nil)
	_ Value = (*LongDoubleValue)(nil)
	_ Value = (*UnknownValue)(nil)
	_ Value = (*ZeroValue)(nil)
	_ Value = Complex128Value(0)
	_ Value = Complex64Value(0)
	_ Value = Float64Value(0)
	_ Value = Int64Value(0)
	_ Value = StringValue("")
	_ Value = UInt64Value(0)
	_ Value = UTF16StringValue(nil)
	_ Value = UTF32StringValue(nil)
	_ Value = VoidValue{}
)

var (
	// Unknown is a singleton representing an undetermined value.  Unknown is
	// comparable.
	Unknown Value = &UnknownValue{}

	// Zero is a singleton representing a zero value of a type. Zero is comparable.
	Zero Value = &ZeroValue{}

	int1 = Int64Value(1)
	int0 = Int64Value(0)
)

type valuer struct{ val Value }

// Value returns the value of a node or UnknownValue if it is undetermined. The
// dynamic type of a Value is one of
//
//	*ComplexLongDoubleValue
//	*LongDoubleValue
//	*UnknownValue
//	*ZeroValue
//	Complex128Value
//	Complex64Value
//	Float64Value
//	Int64Value
//	StringValue
//	UInt64Value
//	UTF16StringValue
//	UTF32StringValue
//	VoidValue
func (v valuer) Value() Value {
	if v.val != nil {
		return v.val
	}

	return Unknown
}

type Value interface {
	isValue()
}

type UnknownValue struct{}

func (*UnknownValue) isValue() {}

func (*UnknownValue) String() string { return "<unknown value>" }

type ZeroValue struct{}

func (*ZeroValue) isValue() {}

func (*ZeroValue) String() string { return "{}" }

type ComplexLongDoubleValue struct {
	Re *LongDoubleValue
	Im *LongDoubleValue
}

func (n *ComplexLongDoubleValue) isValue() {}

type LongDoubleValue big.Float

func (*LongDoubleValue) isValue() {}

type Complex128Value complex128

func (Complex128Value) isValue() {}

type Complex64Value complex64

func (Complex64Value) isValue() {}

type Float64Value float64

func (Float64Value) isValue() {}

type Int64Value int64

func (Int64Value) isValue() {}

type UInt64Value uint64

func (UInt64Value) isValue() {}

type VoidValue struct{}

func (VoidValue) isValue() {}

type StringValue string

func (StringValue) isValue() {}

type UTF16StringValue []uint16

func (UTF16StringValue) isValue() {}

type UTF32StringValue []rune

func (UTF32StringValue) isValue() {}

func (n *ConstantExpression) eval(c *ctx, mode flags) (r Value) {
	n.val = n.ConditionalExpression.eval(c, mode)
	return n.Value()
}

func (n *ConditionalExpression) eval(c *ctx, mode flags) (r Value) {
	if mode.has(addrOf) {
		c.errors.add(errorf("TODO %v %v", n.Case, mode.has(addrOf)))
		return n.Value()
	}

	switch n.Case {
	case ConditionalExpressionLOr: // LogicalOrExpression
		n.val = n.LogicalOrExpression.eval(c, mode)
	case ConditionalExpressionCond: // LogicalOrExpression '?' ExpressionList ':' ConditionalExpression
		switch val := n.LogicalOrExpression.eval(c, mode); {
		case isNonzero(val):
			n.val = c.convert(n.ExpressionList.eval(c, mode), n.Type())
		case isZero(val):
			n.val = c.convert(n.ConditionalExpression.eval(c, mode), n.Type())
		}
	default:
		c.errors.add(errorf("internal error: %v", n.Case))
	}
	return n.Value()
}

func (n *BinaryExpression) eval(c *ctx, mode flags) (r Value) {
	if mode.has(addrOf) {
		c.errors.add(errorf("TODO %v %v", n.Op, mode.has(addrOf)))
		return n.Value()
	}

	switch n.Op {
	case BinaryOperationAdd:
		switch x := c.convert(n.Lhs.eval(c, mode), n.Type()).(type) {
		case *UnknownValue:
			// nop
		case Int64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), n.Type()).(type) {
			case *UnknownValue:
				// nop
			case Int64Value:
				n.val = c.convert(x+y, n.Type())
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		case UInt64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), n.Type()).(type) {
			case *UnknownValue:
				// nop
			case UInt64Value:
				n.val = c.convert(x+y, n.Type())
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		case Float64Value:
			// nop
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Op, x))
		}
	case BinaryOperationSub:
		switch x := c.convert(n.Lhs.eval(c, mode), n.Type()).(type) {
		case *UnknownValue:
			// nop
		case UInt64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), n.Type()).(type) {
			case *UnknownValue:
				// nop
			case UInt64Value:
				n.val = c.convert(x-y, n.Type())
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		case Int64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), n.Type()).(type) {
			case *UnknownValue:
				// nop
			case Int64Value:
				n.val = c.convert(x-y, n.Type())
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Op, x))
		}
	case BinaryOperationMul:
		switch x := c.convert(n.Lhs.eval(c, mode), n.Type()).(type) {
		case *UnknownValue:
			// nop
		case UInt64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), n.Type()).(type) {
			case *UnknownValue:
				// nop
			case UInt64Value:
				n.val = c.convert(x*y, n.Type())
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		case Int64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), n.Type()).(type) {
			case *UnknownValue:
				// nop
			case Int64Value:
				n.val = c.convert(x*y, n.Type())
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Op, x))
		}
	case BinaryOperationDiv:
		switch x := c.convert(n.Lhs.eval(c, mode), n.Type()).(type) {
		case *UnknownValue:
			// nop
		case Int64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), n.Type()).(type) {
			case *UnknownValue:
				// nop
			case Int64Value:
				if y != 0 {
					n.val = c.convert(x/y, n.Type())
					break
				}
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		case UInt64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), n.Type()).(type) {
			case *UnknownValue:
				// nop
			case UInt64Value:
				if y != 0 {
					n.val = c.convert(x/y, n.Type())
					break
				}
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Op, x))
		}
	case BinaryOperationMod:
		switch x := c.convert(n.Lhs.eval(c, mode), n.Type()).(type) {
		case *UnknownValue:
			// nop
		case Int64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), n.Type()).(type) {
			case *UnknownValue:
				// nop
			case Int64Value:
				if y != 0 {
					n.val = c.convert(x%y, n.Type())
					break
				}
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		case UInt64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), n.Type()).(type) {
			case *UnknownValue:
				// nop
			case UInt64Value:
				if y != 0 {
					n.val = c.convert(x%y, n.Type())
					break
				}
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Op, x))
		}
	case BinaryOperationLOr:
		switch v := c.convert(n.Lhs.eval(c, mode), n.Type()); {
		case isZero(v):
			switch v := c.convert(n.Rhs.eval(c, mode), n.Type()); {
			case isZero(v):
				n.val = int0
			case isNonzero(v):
				n.val = int1
			}
		case isNonzero(v):
			n.val = int1
		}
	case BinaryOperationLAnd:
		switch v := n.Lhs.eval(c, mode); {
		case isZero(v):
			n.val = int0
		case isNonzero(v):
			switch w := n.Rhs.eval(c, mode); {
			case isZero(w):
				n.val = int0
			case isNonzero(w):
				n.val = int1
			}
		}
	case BinaryOperationOr:
		switch x := c.convert(n.Lhs.eval(c, mode), n.Type()).(type) {
		case *UnknownValue:
			// nop
		case Int64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), n.Type()).(type) {
			case *UnknownValue:
				// nop
			case Int64Value:
				n.val = c.convert(x|y, n.Type())
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		case UInt64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), n.Type()).(type) {
			case *UnknownValue:
				// nop
			case UInt64Value:
				n.val = c.convert(x|y, n.Type())
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Op, x))
		}
	case BinaryOperationXor:
		switch x := c.convert(n.Lhs.eval(c, mode), n.Type()).(type) {
		case *UnknownValue:
			// ok
		case Int64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), n.Type()).(type) {
			case *UnknownValue:
				// ok
			case Int64Value:
				n.val = c.convert(x^y, n.Type())
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		case UInt64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), n.Type()).(type) {
			case *UnknownValue:
				// ok
			case UInt64Value:
				n.val = c.convert(x^y, n.Type())
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Op, x))
		}
	case BinaryOperationAnd:
		switch x := c.convert(n.Lhs.eval(c, mode), n.Type()).(type) {
		case *UnknownValue:
			// ok
		case Int64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), n.Type()).(type) {
			case *UnknownValue:
				// ok
			case Int64Value:
				n.val = c.convert(x&y, n.Type())
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		case UInt64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), n.Type()).(type) {
			case *UnknownValue:
				// ok
			case UInt64Value:
				n.val = c.convert(x&y, n.Type())
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Op, x))
		}
	case BinaryOperationEq:
		t1 := n.Lhs.Type()
		t2 := n.Rhs.Type()
		if IsArithmeticType(t1) && IsArithmeticType(t2) {
			t1 = UsualArithmeticConversions(t1, t2)
			t2 = t1
		}
		switch x := c.convert(n.Lhs.eval(c, mode), t1).(type) {
		case *UnknownValue:
			// ok
		case Int64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), t2).(type) {
			case *UnknownValue:
				// ok
			case Int64Value:
				n.val = bool2int(x == y)
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		case UInt64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), t2).(type) {
			case *UnknownValue:
				// ok
			case UInt64Value:
				n.val = bool2int(x == y)
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Op, x))
		}
	case BinaryOperationNeq:
		t1 := n.Lhs.Type()
		t2 := n.Rhs.Type()
		if IsArithmeticType(t1) && IsArithmeticType(t2) {
			t1 = UsualArithmeticConversions(t1, t2)
			t2 = t1
		}
		switch x := c.convert(n.Lhs.eval(c, mode), t1).(type) {
		case *UnknownValue:
			// ok
		case Int64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), t2).(type) {
			case *UnknownValue:
				// ok
			case Int64Value:
				n.val = bool2int(x != y)
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		case UInt64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), t2).(type) {
			case *UnknownValue:
				// ok
			case UInt64Value:
				n.val = bool2int(x != y)
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Op, x))
		}
	case BinaryOperationLt:
		t1 := n.Lhs.Type()
		t2 := n.Rhs.Type()
		if IsArithmeticType(t1) && IsArithmeticType(t2) {
			t1 = UsualArithmeticConversions(t1, t2)
			t2 = t1
		}
		switch x := c.convert(n.Lhs.eval(c, mode), t1).(type) {
		case *UnknownValue:
			// ok
		case Int64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), t1).(type) {
			case *UnknownValue:
				// ok
			case Int64Value:
				if x < y {
					n.val = int1
					break
				}

				n.val = int0
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		case UInt64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), t2).(type) {
			case *UnknownValue:
				// ok
			case UInt64Value:
				if x < y {
					n.val = int1
					break
				}

				n.val = int0
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Op, x))
		}
	case BinaryOperationGt:
		t1 := n.Lhs.Type()
		t2 := n.Rhs.Type()
		if IsArithmeticType(t1) && IsArithmeticType(t2) {
			t1 = UsualArithmeticConversions(t1, t2)
			t2 = t1
		}
		switch x := c.convert(n.Lhs.eval(c, mode), t1).(type) {
		case *UnknownValue:
			// ok
		case Int64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), t2).(type) {
			case *UnknownValue:
				// ok
			case Int64Value:
				if x > y {
					n.val = int1
					break
				}

				n.val = int0
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		case UInt64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), t2).(type) {
			case *UnknownValue:
				// ok
			case UInt64Value:
				if x > y {
					n.val = int1
					break
				}

				n.val = int0
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Op, x))
		}
	case BinaryOperationLeq:
		t1 := n.Lhs.Type()
		t2 := n.Rhs.Type()
		if IsArithmeticType(t1) && IsArithmeticType(t2) {
			t1 = UsualArithmeticConversions(t1, t2)
			t2 = t1
		}
		switch x := c.convert(n.Lhs.eval(c, mode), t1).(type) {
		case *UnknownValue:
			// ok
		case Int64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), t2).(type) {
			case *UnknownValue:
				// ok
			case Int64Value:
				if x <= y {
					n.val = int1
					break
				}

				n.val = int0
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		case UInt64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), t2).(type) {
			case *UnknownValue:
				// ok
			case UInt64Value:
				if x <= y {
					n.val = int1
					break
				}

				n.val = int0
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Op, x))
		}
	case BinaryOperationGeq:
		t1 := n.Lhs.Type()
		t2 := n.Rhs.Type()
		if IsArithmeticType(t1) && IsArithmeticType(t2) {
			t1 = UsualArithmeticConversions(t1, t2)
			t2 = t1
		}
		switch x := c.convert(n.Lhs.eval(c, mode), t1).(type) {
		case *UnknownValue:
			// ok
		case Int64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), t2).(type) {
			case *UnknownValue:
				// ok
			case Int64Value:
				if x >= y {
					n.val = int1
					break
				}

				n.val = int0
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		case UInt64Value:
			switch y := c.convert(n.Rhs.eval(c, mode), t2).(type) {
			case *UnknownValue:
				// ok
			case UInt64Value:
				if x >= y {
					n.val = int1
					break
				}

				n.val = int0
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Op, x))
		}
	case BinaryOperationLsh:
		switch x := c.convert(n.Lhs.eval(c, mode), n.Type()).(type) {
		case *UnknownValue:
			// nop
		case Int64Value:
			switch y := n.Rhs.eval(c, mode).(type) {
			case *UnknownValue:
				// nop
			case Int64Value:
				n.val = c.convert(x<<y, n.Type())
			case UInt64Value:
				n.val = c.convert(x<<y, n.Type())
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		case UInt64Value:
			switch y := n.Rhs.eval(c, mode).(type) {
			case *UnknownValue:
				// nop
			case Int64Value:
				n.val = c.convert(x<<y, n.Type())
			case UInt64Value:
				n.val = c.convert(x<<y, n.Type())
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Op, x))
		}
	case BinaryOperationRsh:
		switch x := c.convert(n.Lhs.eval(c, mode), n.Type()).(type) {
		case *UnknownValue:
			// nop
		case Int64Value:
			switch y := n.Rhs.eval(c, mode).(type) {
			case *UnknownValue:
				// nop
			case Int64Value:
				n.val = c.convert(x>>y, n.Type())
			case UInt64Value:
				n.val = c.convert(x>>y, n.Type())
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		case UInt64Value:
			switch y := n.Rhs.eval(c, mode).(type) {
			case *UnknownValue:
				// nop
			case Int64Value:
				n.val = c.convert(x>>y, n.Type())
			case UInt64Value:
				n.val = c.convert(x>>y, n.Type())
			default:
				c.errors.add(errorf("TODO %v TYPE %T", n.Op, y))
			}
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Op, x))
		}
	default:
		c.errors.add(errorf("internal error: %v", n.Op))
	}
	return n.Value()
}

func (n *CastExpression) eval(c *ctx, mode flags) (r Value) {
	if mode.has(addrOf) {
		switch n.Case {
		case CastExpressionUnary: // UnaryExpression
			n.val = n.UnaryExpression.eval(c, mode)
		case CastExpressionCast: // '(' TypeName ')' CastExpression
			c.errors.add(errorf("TODO %v %v", n.Case, mode.has(addrOf)))
		default:
			c.errors.add(errorf("internal error: %v", n.Case))
		}
		return n.Value()
	}

	switch n.Case {
	case CastExpressionUnary: // UnaryExpression
		n.val = n.UnaryExpression.eval(c, mode)
	case CastExpressionCast: // '(' TypeName ')' CastExpression
		n.val = c.convert(n.CastExpression.eval(c, mode), n.TypeName.Type())
	default:
		c.errors.add(errorf("internal error: %v", n.Case))
	}
	return n.Value()
}

func (n *UnaryExpression) eval(c *ctx, mode flags) (r Value) {
	if mode.has(addrOf) {
		switch n.Case {
		case UnaryExpressionPostfix: // PostfixExpression
			c.errors.add(errorf("TODO %T %v", n, n.Case))
		case UnaryExpressionInc: // "++" UnaryExpression
			c.errors.add(errorf("TODO %T %v", n, n.Case))
		case UnaryExpressionDec: // "--" UnaryExpression
			c.errors.add(errorf("TODO %T %v", n, n.Case))
		case UnaryExpressionAddrof: // '&' CastExpression
			c.errors.add(errorf("TODO %T %v", n, n.Case))
		case UnaryExpressionDeref: // '*' CastExpression
			switch x := n.CastExpression.eval(c, mode.del(addrOf)).(type) {
			case *UnknownValue:
				// ok
			case UInt64Value:
				if _, ok := n.CastExpression.Type().(*PointerType); ok {
					n.val = c.convert(x, n.CastExpression.Type())
				}
			default:
				c.errors.add(errorf("TODO %v %v %T", n.Case, mode.has(addrOf), x))
			}
		case UnaryExpressionPlus: // '+' CastExpression
			c.errors.add(errorf("TODO %T %v", n, n.Case))
		case UnaryExpressionMinus: // '-' CastExpression
			c.errors.add(errorf("TODO %T %v", n, n.Case))
		case UnaryExpressionCpl: // '~' CastExpression
			c.errors.add(errorf("TODO %T %v", n, n.Case))
		case UnaryExpressionNot: // '!' CastExpression
			c.errors.add(errorf("TODO %T %v", n, n.Case))
		case UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
			c.errors.add(errorf("TODO %T %v", n, n.Case))
		case UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
			c.errors.add(errorf("TODO %T %v", n, n.Case))
		case UnaryExpressionLabelAddr: // "&&" IDENTIFIER
			c.errors.add(errorf("TODO %T %v", n, n.Case))
		case UnaryExpressionAlignofExpr: // "_Alignof" UnaryExpression
			c.errors.add(errorf("TODO %T %v", n, n.Case))
		case UnaryExpressionAlignofType: // "_Alignof" '(' TypeName ')'
			c.errors.add(errorf("TODO %T %v", n, n.Case))
		case UnaryExpressionImag: // "__imag__" UnaryExpression
			c.errors.add(errorf("TODO %T %v", n, n.Case))
		case UnaryExpressionReal: // "__real__" UnaryExpression
			switch x := n.UnaryExpression.eval(c, mode.del(addrOf)).(type) {
			case *UnknownValue:
				// ok
			default:
				c.errors.add(errorf("TODO %v %v %T", n.Case, mode.has(addrOf), x))
			}
		default:
			c.errors.add(errorf("internal error: %v", n.Case))
		}
		return n.Value()
	}

	switch n.Case {
	case UnaryExpressionPostfix: // PostfixExpression
		n.val = n.PostfixExpression.eval(c, mode)
	case UnaryExpressionInc: // "++" UnaryExpression
		// nop
	case UnaryExpressionDec: // "--" UnaryExpression
		// nop
	case UnaryExpressionAddrof: // '&' CastExpression
		n.val = c.convert(n.CastExpression.eval(c, mode.add(addrOf)), n.Type())
	case UnaryExpressionDeref: // '*' CastExpression
		// nop
	case UnaryExpressionPlus: // '+' CastExpression
		switch x := c.convert(n.CastExpression.eval(c, mode), n.Type()).(type) {
		case *UnknownValue:
			// nop
		case Int64Value:
			n.val = c.convert(x, n.Type())
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Case, x))
		}
	case UnaryExpressionMinus: // '-' CastExpression
		switch x := c.convert(n.CastExpression.eval(c, mode), n.Type()).(type) {
		case *UnknownValue:
			// nop
		case Int64Value:
			n.val = c.convert(-x, n.Type())
		case UInt64Value:
			n.val = c.convert(-x, n.Type())
		case Float64Value:
			// nop
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Case, x))
		}
	case UnaryExpressionCpl: // '~' CastExpression
		switch x := n.CastExpression.eval(c, mode).(type) {
		case *UnknownValue:
			// nop
		case Int64Value:
			n.val = c.convert(^x, n.Type())
		case UInt64Value:
			n.val = c.convert(^x, n.Type())
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Case, x))
		}
	case UnaryExpressionNot: // '!' CastExpression
		switch x := n.CastExpression.eval(c, mode).(type) {
		case *UnknownValue:
			// nop
		case Int64Value:
			n.val = c.convert(bool2int(x == 0), n.Type())
		case UInt64Value:
			n.val = c.convert(bool2int(x == 0), n.Type())
		case StringValue, UTF16StringValue, UTF32StringValue:
			n.val = c.convert(int0, n.Type())
		default:
			c.errors.add(errorf("TODO %v TYPE %T", n.Case, x))
		}
	case UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
		// nop
	case UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
		// nop
	case UnaryExpressionLabelAddr: // "&&" IDENTIFIER
		// nop
	case UnaryExpressionAlignofExpr: // "_Alignof" UnaryExpression
		// nop
	case UnaryExpressionAlignofType: // "_Alignof" '(' TypeName ')'
		// nop
	case UnaryExpressionImag: // "__imag__" UnaryExpression
		// nop
	case UnaryExpressionReal: // "__real__" UnaryExpression
		// nop
	default:
		c.errors.add(errorf("internal error: %v", n.Case))
	}
	return n.Value()
}

func (n *PostfixExpression) eval(c *ctx, mode flags) (r Value) {
	if mode.has(addrOf) {
		switch n.Case {
		case PostfixExpressionPrimary: // PrimaryExpression
			c.errors.add(errorf("TODO %v %v", n.Case, mode.has(addrOf)))
		case PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
			switch x := n.PostfixExpression.eval(c, mode.del(addrOf)).(type) {
			case *UnknownValue, StringValue, UTF16StringValue, UTF32StringValue:
				// ok
			case UInt64Value:
				if p, ok := n.PostfixExpression.Type().(*PointerType); ok {
					if ix, ok := int64Value(c, n.ExpressionList); ok && ix >= 0 {
						if esz := p.Elem().Size(); esz >= 0 {
							n.val = c.convert(x+UInt64Value(ix*esz), c.newPointerType(p.Elem()))
						}
					}
				}
			default:
				c.errors.add(errorf("TODO %v %v %T", n.Case, mode.has(addrOf), x))
			}
		case PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
			c.errors.add(errorf("TODO %v %v", n.Case, mode.has(addrOf)))
		case PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
			switch x := n.PostfixExpression.Type().(type) {
			case *StructType:
				switch y := n.PostfixExpression.eval(c, mode).(type) {
				case *UnknownValue:
					// ok
				case UInt64Value:
					if f := x.FieldByName(n.Token2.SrcStr()); f != nil {
						n.val = c.convert(y+UInt64Value(f.Offset()), c.newPointerType(f.Type()))
					}
				default:
					c.errors.add(errorf("TODO %T %T", x, y))
				}
			case *UnionType:
				switch y := n.PostfixExpression.eval(c, mode).(type) {
				case *UnknownValue:
					// ok
				case UInt64Value:
					if f := x.FieldByName(n.Token2.SrcStr()); f != nil {
						n.val = c.convert(y+UInt64Value(f.Offset()), c.newPointerType(f.Type()))
					}
				default:
					c.errors.add(errorf("TODO %T %T", x, y))
				}
			default:
				c.errors.add(errorf("TODO %T", x))
			}
		case PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
			switch x := n.PostfixExpression.Type().(type) {
			case *PointerType:
				switch y := n.PostfixExpression.eval(c, mode.del(addrOf)).(type) {
				case *UnknownValue:
					// ok
				case UInt64Value:
					switch z := x.Elem().(type) {
					case *StructType:
						if f := z.FieldByName(n.Token2.SrcStr()); f != nil {
							n.val = c.convert(y+UInt64Value(f.Offset()), c.newPointerType(f.Type()))
						}
					case *UnionType:
						if f := z.FieldByName(n.Token2.SrcStr()); f != nil {
							n.val = c.convert(y+UInt64Value(f.Offset()), c.newPointerType(f.Type()))
						}
					default:
						c.errors.add(errorf("TODO %T %T", x, y, z))
					}
				default:
					c.errors.add(errorf("TODO %T %T", x, y))
				}
			default:
				c.errors.add(errorf("TODO %T", x))
			}
		case PostfixExpressionInc: // PostfixExpression "++"
			c.errors.add(errorf("TODO %v %v", n.Case, mode.has(addrOf)))
		case PostfixExpressionDec: // PostfixExpression "--"
			c.errors.add(errorf("TODO %v %v", n.Case, mode.has(addrOf)))
		case PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
			// ok
		default:
			c.errors.add(errorf("internal error: %v", n.Case))
		}
		return n.Value()
	}

	switch n.Case {
	case PostfixExpressionPrimary: // PrimaryExpression
		n.val = n.PrimaryExpression.eval(c, mode)
	case PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		switch {
		case isPointerType(n.PostfixExpression.Type()) && IsIntegerType(n.ExpressionList.Type()):
			switch v := n.PostfixExpression.eval(c, mode).(type) {
			case *UnknownValue:
				// nop
			case StringValue:
				switch x := n.ExpressionList.eval(c, 0).(type) {
				case *UnknownValue:
					// nop
				case Int64Value:
					if x >= 0 && x < Int64Value(len(v)) {
						n.val = c.convert(Int64Value(v[x]), n.Type())
					}
				case UInt64Value:
					if x < UInt64Value(len(v)) {
						n.val = c.convert(Int64Value(v[x]), n.Type())
					}
				default:
					c.errors.add(errorf("TODO %v %T", n.Case, x))
				}
			case UTF32StringValue:
				switch x := n.ExpressionList.eval(c, 0).(type) {
				case *UnknownValue:
					// nop
				case Int64Value:
					if x >= 0 && x < Int64Value(len(v)) {
						n.val = c.convert(Int64Value(v[x]), n.Type())
					}
				case UInt64Value:
					if x < UInt64Value(len(v)) {
						n.val = c.convert(Int64Value(v[x]), n.Type())
					}
				default:
					c.errors.add(errorf("TODO %v %T", n.Case, x))
				}
			case UTF16StringValue:
				switch x := n.ExpressionList.eval(c, 0).(type) {
				case *UnknownValue:
					// nop
				case Int64Value:
					if x >= 0 && x < Int64Value(len(v)) {
						n.val = c.convert(Int64Value(v[x]), n.Type())
					}
				case UInt64Value:
					if x < UInt64Value(len(v)) {
						n.val = c.convert(Int64Value(v[x]), n.Type())
					}
				default:
					c.errors.add(errorf("TODO %v %T", n.Case, x))
				}
			case UInt64Value:
				// nop
			default:
				// trc("%v: %v %v [%v %v] %T", n.Token.Position(), n.PostfixExpression.Value(), n.PostfixExpression.Type(), n.ExpressionList.Value(), n.ExpressionList.Type(), v)
				c.errors.add(errorf("TODO %v %T", n.Case, v))
			}
		case IsIntegerType(n.PostfixExpression.Type()) && isPointerType(n.ExpressionList.Type()):
			switch v := n.ExpressionList.eval(c, mode).(type) {
			case *UnknownValue:
				// nop
			case StringValue:
				switch x := n.PostfixExpression.eval(c, 0).(type) {
				case *UnknownValue:
					// nop
				case Int64Value:
					if x >= 0 && x < Int64Value(len(v)) {
						n.val = c.convert(Int64Value(v[x]), n.Type())
					}
				case UInt64Value:
					if x < UInt64Value(len(v)) {
						n.val = c.convert(Int64Value(v[x]), n.Type())
					}
				default:
					c.errors.add(errorf("TODO %v %T", n.Case, x))
				}
			default:
				// trc("%v: %v %v [%v %v] %T", n.Token.Position(), n.PostfixExpression.Value(), n.PostfixExpression.Type(), n.ExpressionList.Value(), n.ExpressionList.Type(), v)
				c.errors.add(errorf("TODO %v %T", n.Case, v))
			}
		}
	case PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		// nop
	case PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		// nop
	case PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		// nop
	case PostfixExpressionInc: // PostfixExpression "++"
		// nop
	case PostfixExpressionDec: // PostfixExpression "--"
		// nop
	case PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		if n.InitializerList == nil || n.InitializerList.InitializerList != nil || n.InitializerList.Initializer.Case != InitializerExpr {
			break
		}

		v := c.convert(n.InitializerList.Initializer.AssignmentExpression.eval(c, mode), n.Type())
		switch n.Type().(type) {
		case *PredefinedType:
			n.val = v
		case *EnumType:
			n.val = v
		case *PointerType:
			n.val = v
		}
	default:
		c.errors.add(errorf("internal error: %v", n.Case))
	}
	return n.Value()
}

func (n *PrimaryExpression) eval(c *ctx, mode flags) (r Value) {
	if mode.has(addrOf) {
		switch n.Case {
		case PrimaryExpressionIdent: // IDENTIFIER
			// nop
		case PrimaryExpressionInt: // INTCONST
			c.errors.add(errorf("TODO %v %v", n.Case, mode.has(addrOf)))
		case PrimaryExpressionFloat: // FLOATCONST
			c.errors.add(errorf("TODO %v %v", n.Case, mode.has(addrOf)))
		case PrimaryExpressionChar: // CHARCONST
			c.errors.add(errorf("TODO %v %v", n.Case, mode.has(addrOf)))
		case PrimaryExpressionLChar: // LONGCHARCONST
			c.errors.add(errorf("TODO %v %v", n.Case, mode.has(addrOf)))
		case PrimaryExpressionString: // STRINGLITERAL
			// ok
		case PrimaryExpressionLString: // LONGSTRINGLITERAL
			c.errors.add(errorf("TODO %v %v", n.Case, mode.has(addrOf)))
		case PrimaryExpressionExpr: // '(' Expression ')'
			n.val = n.ExpressionList.eval(c, mode)
		case PrimaryExpressionStmt: // '(' CompoundStatement ')'
			c.errors.add(errorf("TODO %v %v", n.Case, mode.has(addrOf)))
		case PrimaryExpressionGeneric: // GenericSelection
			c.errors.add(errorf("TODO %v %v", n.Case, mode.has(addrOf)))
		default:
			c.errors.add(errorf("internal error: %v", n.Case))
		}
		return n.Value()
	}

	switch n.Case {
	case PrimaryExpressionIdent: // IDENTIFIER
		switch n.resolvedTo.(type) {
		case *Declarator, *Parameter, *Enumerator, nil:
			// ok
		default:
			c.errors.add(errorf("TODO %v %T", n.Case, n.resolvedTo))
		}
	case PrimaryExpressionInt: // INTCONST
		// nop
	case PrimaryExpressionFloat: // FLOATCONST
		// nop
	case PrimaryExpressionChar: // CHARCONST
		// nop
	case PrimaryExpressionLChar: // LONGCHARCONST
		// nop
	case PrimaryExpressionString: // STRINGLITERAL
		// nop
	case PrimaryExpressionLString: // LONGSTRINGLITERAL
		// nop
	case PrimaryExpressionExpr: // '(' Expression ')'
		n.val = n.ExpressionList.eval(c, mode)
	case PrimaryExpressionStmt: // '(' CompoundStatement ')'
		// nop
	case PrimaryExpressionGeneric: // GenericSelection
		// nop
	default:
		c.errors.add(errorf("internal error: %v", n.Case))
	}
	return n.Value()
}

func (n *ExpressionList) eval(c *ctx, mode flags) (r Value) {
	if n == nil {
		return Unknown
	}

	n0 := n
	for ; n != nil; n = n.ExpressionList {
		n0.typ = n.AssignmentExpression.Type()
		n0.val = n.AssignmentExpression.eval(c, mode)
	}
	return n0.Value()
}

func (n *AssignmentExpression) eval(c *ctx, mode flags) (r Value) {
	if mode.has(addrOf) {
		c.errors.add(errorf("TODO %v %v", n.Case, mode.has(addrOf)))
		return n.Value()
	}

	switch n.Case {
	case AssignmentExpressionCond: // ConditionalExpression
		n.val = n.ConditionalExpression.eval(c, mode)
	case AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		n.val = c.convert(n.AssignmentExpression.eval(c, mode), n.UnaryExpression.Type())
	case AssignmentExpressionMul, // UnaryExpression "*=" AssignmentExpression
		AssignmentExpressionDiv, // UnaryExpression "/=" AssignmentExpression
		AssignmentExpressionMod, // UnaryExpression "%=" AssignmentExpression
		AssignmentExpressionAdd, // UnaryExpression "+=" AssignmentExpression
		AssignmentExpressionSub, // UnaryExpression "-=" AssignmentExpression
		AssignmentExpressionLsh, // UnaryExpression "<<=" AssignmentExpression
		AssignmentExpressionRsh, // UnaryExpression ">>=" AssignmentExpression
		AssignmentExpressionAnd, // UnaryExpression "&=" AssignmentExpression
		AssignmentExpressionXor, // UnaryExpression "^=" AssignmentExpression
		AssignmentExpressionOr:  // UnaryExpression "|=" AssignmentExpression

		n.UnaryExpression.eval(c, mode)
		n.AssignmentExpression.eval(c, mode)
	default:
		c.errors.add(errorf("internal error: %v", n.Case))
	}
	return n.Value()
}

func isZero(v Value) bool {
	switch x := v.(type) {
	case *UnknownValue:
		return false
	case Int64Value:
		return x == 0
	case UInt64Value:
		return x == 0
	default:
		panic(todo("%T", x))
	}
}

func isNonzero(v Value) bool {
	switch x := v.(type) {
	case *UnknownValue:
		return false
	case Int64Value:
		return x != 0
	case UInt64Value:
		return x != 0
	default:
		panic(todo("%T", x))
	}
}
