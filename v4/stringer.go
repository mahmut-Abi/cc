// Code generated by "stringer -output stringer.go -linecomment -type=tokCh,Kind,Linkage,StorageDuration"; DO NOT EDIT.

package cc

import "strconv"

func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[ADDASSIGN-57345]
	_ = x[ALIGNAS-57346]
	_ = x[ALIGNOF-57347]
	_ = x[ANDAND-57348]
	_ = x[ANDASSIGN-57349]
	_ = x[ARROW-57350]
	_ = x[ASM-57351]
	_ = x[ATOMIC-57352]
	_ = x[ATTRIBUTE-57353]
	_ = x[AUTO-57354]
	_ = x[AUTOTYPE-57355]
	_ = x[BOOL-57356]
	_ = x[BREAK-57357]
	_ = x[CASE-57358]
	_ = x[CHAR-57359]
	_ = x[CHARCONST-57360]
	_ = x[COMPLEX-57361]
	_ = x[CONST-57362]
	_ = x[CONTINUE-57363]
	_ = x[DDD-57364]
	_ = x[DEC-57365]
	_ = x[DECIMAL128-57366]
	_ = x[DECIMAL32-57367]
	_ = x[DECIMAL64-57368]
	_ = x[DECLSPEC-57369]
	_ = x[DEFAULT-57370]
	_ = x[DIVASSIGN-57371]
	_ = x[DO-57372]
	_ = x[DOUBLE-57373]
	_ = x[ELSE-57374]
	_ = x[ENUM-57375]
	_ = x[EQ-57376]
	_ = x[EXTERN-57377]
	_ = x[FLOAT-57378]
	_ = x[FLOAT128-57379]
	_ = x[FLOAT128X-57380]
	_ = x[FLOAT16-57381]
	_ = x[FLOAT32-57382]
	_ = x[FLOAT32X-57383]
	_ = x[FLOAT64-57384]
	_ = x[FLOAT64X-57385]
	_ = x[FLOATCONST-57386]
	_ = x[FOR-57387]
	_ = x[GENERIC-57388]
	_ = x[GEQ-57389]
	_ = x[GOTO-57390]
	_ = x[HEADER_NAME-57391]
	_ = x[IDENTIFIER-57392]
	_ = x[IF-57393]
	_ = x[IMAG-57394]
	_ = x[IMAGINARY-57395]
	_ = x[INC-57396]
	_ = x[INLINE-57397]
	_ = x[INT-57398]
	_ = x[INT128-57399]
	_ = x[INTCONST-57400]
	_ = x[LABEL-57401]
	_ = x[LEQ-57402]
	_ = x[LONG-57403]
	_ = x[LONGCHARCONST-57404]
	_ = x[LONGSTRINGLITERAL-57405]
	_ = x[LSH-57406]
	_ = x[LSHASSIGN-57407]
	_ = x[MODASSIGN-57408]
	_ = x[MULASSIGN-57409]
	_ = x[NEQ-57410]
	_ = x[NONNULL-57411]
	_ = x[NORETURN-57412]
	_ = x[ORASSIGN-57413]
	_ = x[OROR-57414]
	_ = x[PPNUMBER-57415]
	_ = x[PPPASTE-57416]
	_ = x[REAL-57417]
	_ = x[REGISTER-57418]
	_ = x[RESTRICT-57419]
	_ = x[RETURN-57420]
	_ = x[RSH-57421]
	_ = x[RSHASSIGN-57422]
	_ = x[SHORT-57423]
	_ = x[SIGNED-57424]
	_ = x[SIZEOF-57425]
	_ = x[STATIC-57426]
	_ = x[STATICASSERT-57427]
	_ = x[STRINGLITERAL-57428]
	_ = x[STRUCT-57429]
	_ = x[SUBASSIGN-57430]
	_ = x[SWITCH-57431]
	_ = x[THREADLOCAL-57432]
	_ = x[TYPEDEF-57433]
	_ = x[TYPENAME-57434]
	_ = x[TYPEOF-57435]
	_ = x[UINT128-57436]
	_ = x[UNION-57437]
	_ = x[UNSIGNED-57438]
	_ = x[VOID-57439]
	_ = x[VOLATILE-57440]
	_ = x[WHILE-57441]
	_ = x[XORASSIGN-57442]
}

const _tokCh_name = "'+=''_Alignas''_Alignof''&&''&=''->''__asm__''_Atomic''__attribute__''auto''__auto_type''_Bool''break''case''char'character constant'_Complex''const''continue''...''--''_Decimal128''_Decimal32''_Decimal64''__declspec''default''/=''do''double''else''enum''==''extern''float''_Float128''_Float128x''_Float16''_Float32''_Float32x''_Float64''_Float64x'floating point constant'for''_Generic''>=''goto'<header-name>identifier'if''__imag__''_Imaginary''++''inline''int''__int128'integer constant'__label__''<=''long'long character constantlong string literal'<<''<<=''%=''*=''!=''_Nonnull''_Noreturn''|=''||'preprocessing number'##''__real__''register''restrict''return''>>''>>=''short''signed''sizeof''static'_Static_assertstring literal'struct''-=''switch''_Thread_local''typedef'type name'typeof''__uint128_t''union''unsigned''void''volatile''while''^='"

var _tokCh_index = [...]uint16{0, 4, 14, 24, 28, 32, 36, 45, 54, 69, 75, 88, 95, 102, 108, 114, 132, 142, 149, 159, 164, 168, 181, 193, 205, 217, 226, 230, 234, 242, 248, 254, 258, 266, 273, 284, 296, 306, 316, 327, 337, 348, 371, 376, 386, 390, 396, 409, 419, 423, 433, 445, 449, 457, 462, 472, 488, 499, 503, 509, 532, 551, 555, 560, 564, 568, 572, 582, 593, 597, 601, 621, 625, 635, 645, 655, 663, 667, 672, 679, 687, 695, 703, 717, 731, 739, 743, 751, 766, 775, 784, 792, 805, 812, 822, 828, 838, 845, 849}

func (i tokCh) String() string {
	i -= 57345
	if i < 0 || i >= tokCh(len(_tokCh_index)-1) {
		return "tokCh(" + strconv.FormatInt(int64(i+57345), 10) + ")"
	}
	return _tokCh_name[_tokCh_index[i]:_tokCh_index[i+1]]
}
func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[InvalidKind-0]
	_ = x[Array-1]
	_ = x[Bool-2]
	_ = x[Char-3]
	_ = x[ComplexChar-4]
	_ = x[ComplexDouble-5]
	_ = x[ComplexFloat-6]
	_ = x[ComplexInt-7]
	_ = x[ComplexLong-8]
	_ = x[ComplexLongDouble-9]
	_ = x[ComplexLongLong-10]
	_ = x[ComplexShort-11]
	_ = x[ComplexUInt-12]
	_ = x[ComplexUShort-13]
	_ = x[Decimal128-14]
	_ = x[Decimal32-15]
	_ = x[Decimal64-16]
	_ = x[Double-17]
	_ = x[Enum-18]
	_ = x[Float-19]
	_ = x[Float128-20]
	_ = x[Float128x-21]
	_ = x[Float16-22]
	_ = x[Float32-23]
	_ = x[Float32x-24]
	_ = x[Float64-25]
	_ = x[Float64x-26]
	_ = x[Function-27]
	_ = x[Int-28]
	_ = x[Int128-29]
	_ = x[Long-30]
	_ = x[LongDouble-31]
	_ = x[LongLong-32]
	_ = x[Ptr-33]
	_ = x[SChar-34]
	_ = x[Short-35]
	_ = x[Struct-36]
	_ = x[UChar-37]
	_ = x[UInt-38]
	_ = x[UInt128-39]
	_ = x[ULong-40]
	_ = x[ULongLong-41]
	_ = x[UShort-42]
	_ = x[Union-43]
	_ = x[Void-44]
	_ = x[maxKind-45]
}

const _Kind_name = "InvalidKindarray_Boolchar_Complex char_Complex double_Complex float_Complex int_Complex long_Complex long double_Complex long long_Complex short_Complex unsigned_Complex unsigned short_Decimal128_Decimal32_Decimal64doubleenumfloat_Float128_Float128x_Float16_Float32_Float32x_Float64_Float64xfunctionint__int128longlong doublelong longpointersigned charshortstructunsigned charunsignedunsigned __int128unsigned longunsigned long longunsigned shortunionvoidmaxKind"

var _Kind_index = [...]uint16{0, 11, 16, 21, 25, 38, 53, 67, 79, 92, 112, 130, 144, 161, 184, 195, 205, 215, 221, 225, 230, 239, 249, 257, 265, 274, 282, 291, 299, 302, 310, 314, 325, 334, 341, 352, 357, 363, 376, 384, 401, 414, 432, 446, 451, 455, 462}

func (i Kind) String() string {
	if i < 0 || i >= Kind(len(_Kind_index)-1) {
		return "Kind(" + strconv.FormatInt(int64(i), 10) + ")"
	}
	return _Kind_name[_Kind_index[i]:_Kind_index[i+1]]
}
func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[External-0]
	_ = x[Internal-1]
	_ = x[None-2]
}

const _Linkage_name = "ExternalInternalNone"

var _Linkage_index = [...]uint8{0, 8, 16, 20}

func (i Linkage) String() string {
	if i < 0 || i >= Linkage(len(_Linkage_index)-1) {
		return "Linkage(" + strconv.FormatInt(int64(i), 10) + ")"
	}
	return _Linkage_name[_Linkage_index[i]:_Linkage_index[i+1]]
}
func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[Static-0]
	_ = x[Automatic-1]
	_ = x[Allocated-2]
}

const _StorageDuration_name = "StaticAutomaticAllocated"

var _StorageDuration_index = [...]uint8{0, 6, 15, 24}

func (i StorageDuration) String() string {
	if i < 0 || i >= StorageDuration(len(_StorageDuration_index)-1) {
		return "StorageDuration(" + strconv.FormatInt(int64(i), 10) + ")"
	}
	return _StorageDuration_name[_StorageDuration_index[i]:_StorageDuration_index[i+1]]
}
