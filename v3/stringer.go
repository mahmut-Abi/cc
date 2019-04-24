// Code generated by "stringer -output stringer.go -linecomment -type=Kind"; DO NOT EDIT.

package cc

import "strconv"

func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[Invalid-0]
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
	_ = x[Float32-21]
	_ = x[Float32x-22]
	_ = x[Float64-23]
	_ = x[Float64x-24]
	_ = x[Int-25]
	_ = x[Int128-26]
	_ = x[Long-27]
	_ = x[LongDouble-28]
	_ = x[LongLong-29]
	_ = x[Ptr-30]
	_ = x[SChar-31]
	_ = x[Short-32]
	_ = x[Struct-33]
	_ = x[TypedefName-34]
	_ = x[UChar-35]
	_ = x[UInt-36]
	_ = x[UInt128-37]
	_ = x[ULong-38]
	_ = x[ULongLong-39]
	_ = x[UShort-40]
	_ = x[Union-41]
	_ = x[Void-42]
	_ = x[typeofExpr-43]
	_ = x[typeofType-44]
	_ = x[maxKind-45]
}

const _Kind_name = "InvalidT[]_Boolcharcomplex charcomplex doublecomplex floatcomplex intcomplex longcomplex long doublecomplex long longcomplex shortcomplex unsignedcomplex shor_Decimal128_Decimal32_Decimal64doubleenumfloat_Float128_Float32_Float32x_Float64_Float64xint__int128longlong doublelong longpointersigned charchortstructtypedefnameunsigned charunsignedunsigned __int128unsigned longunsigned long longunsigned shortunionvoidtypeofExprtypeofTypemaxKind"

var _Kind_index = [...]uint16{0, 7, 10, 15, 19, 31, 45, 58, 69, 81, 100, 117, 130, 146, 158, 169, 179, 189, 195, 199, 204, 213, 221, 230, 238, 247, 250, 258, 262, 273, 282, 289, 300, 305, 311, 322, 335, 343, 360, 373, 391, 405, 410, 414, 424, 434, 441}

func (i Kind) String() string {
	if i >= Kind(len(_Kind_index)-1) {
		return "Kind(" + strconv.FormatInt(int64(i), 10) + ")"
	}
	return _Kind_name[_Kind_index[i]:_Kind_index[i+1]]
}
