#include "../units/Units.hpp"

#include "Sfinae.hpp"

#include <cassert> /* assert() */
#include <cfenv>   /* fesetround() */

using namespace units;

/*
 * euclid
 */

static_assert(euclid<3, 9>::value == 3,
              "Euclid's algorithm should return the GCD");

static_assert(euclid<7, 13>::value == 1,
              "Euclid's algorithm should return the GCD");

static_assert(euclid<21, 91>::value == 7,
              "Euclid's algorithm should return the GCD");

/*
 * rational, is_rational
 */

static_assert(is_rational<rational<1, 2>>::value, "Test is_rational test");

static_assert(!is_rational<double>::value, "Test is_rational test");

static_assert(is_same_rational<rational<1, 2>, rational<1, 2>>::value,
              "Test is_same_rational test");

static_assert(is_same_rational<rational<1, 2>, rational<33, 66>>::value,
              "Test is_same_rational test");

static_assert(!is_same_rational<rational<1, 3>, rational<33, 66>>::value,
              "Test is_same_rational test");

static_assert(!is_same_rational<rational<1, 2>, double>::value,
              "Test is_same_rational test");

static_assert(!is_same_rational<double, double>::value,
              "Test is_same_rational test");

static_assert(rational<3, 9>::numerator == 1,
              "Rationals should be self-minimising");

static_assert(rational<3, 9>::denominator == 3,
              "Rationals should be self-minimising");

static_assert(double(rational<1, 8>{}) == .125,
              "Test rational casting");

static_assert(double(rational<12, 3>{}) == 4.,
              "Test rational casting");

static_assert(int(rational<12, 3>{}) == 4,
              "Test rational casting");

/*
 * rational_invert_t
 */

static_assert(is_same_rational<rational<1, 2>,
                               rational_invert_t<rational<2, 1>>>::value,
              "Rational inversion");

/*
 * rational_mul_t, rational_div_t
 */

static_assert(is_same_rational<rational_mul_t<rational<3, 5>, rational<2, 7>>,
                               rational<6, 35>>::value,
              "Rational multiplication");

static_assert(is_same_rational<rational_div_t<rational<3, 5>, rational<2, 7>>,
                               rational<21, 10>>::value,
              "Rational division");

/*
 * exponents_chain
 */

static_assert(exponents_chain<23, exponents_chain<42, exponents_chain<13>>>::value == 23,
              "Test exponents_chain value retrieval");

static_assert(exponents_chain<23, exponents_chain<42, exponents_chain<13>>>::tail::value == 42,
              "Test exponents_chain value retrieval");

static_assert(exponents_chain<23, exponents_chain<42, exponents_chain<13>>>::tail::tail::value == 13,
              "Test exponents_chain value retrieval");

/*
 * is_exponents_chain
 */

static_assert(is_exponents_chain<void>::value, "Test is_exponents_chain");

static_assert(is_exponents_chain<exponents_chain<0>>::value, "Test is_exponents_chain");

static_assert(is_exponents_chain<exponents_chain<0, exponents_chain<1>>>::value, "Test is_exponents_chain");

static_assert(!is_exponents_chain<double>::value, "Test is_exponents_chain");

struct fake_exponents_chain {
	typedef void tail;
	static constexpr sint const value{1};
};

static_assert(!is_exponents_chain<fake_exponents_chain>::value,
              "The fake_exponents_chain type is not a exponents_chain instance");

static_assert(!is_exponents_chain<exponents_chain<3, fake_exponents_chain>>::value,
              "The fake_exponents_chain type is not a exponents_chain instance");

struct fail_exponents_chain {
};

static_assert(!is_exponents_chain<fail_exponents_chain>::value,
              "The fail_exponents_chain type is not a exponents_chain instance");

static_assert(!is_exponents_chain<exponents_chain<3, fail_exponents_chain>>::value,
              "The fail_exponents_chain type is not a exponents_chain instance");

/*
 * exponents_t, is_same_length
 */

static_assert(is_same<exponents_t<>, void>::value,
              "Test exponents_chain factory");

static_assert(is_same<exponents_t<0>, exponents_chain<0>>::value,
              "Test exponents_chain factory");

static_assert(is_same<exponents_t<1, 0>, exponents_chain<1, exponents_t<0>>>::value,
              "Test exponents_chain factory");

static_assert(is_same<exponents_t<2, 1, 0>, exponents_chain<2, exponents_t<1, 0>>>::value,
              "Test exponents_chain factory");

static_assert(is_same_length<exponents_t<>, exponents_t<>>::value,
              "Test is_same_length test");

static_assert(is_same_length<exponents_t<1>, exponents_t<2>>::value,
              "Test is_same_length test");

static_assert(is_same_length<exponents_t<1, 1>, exponents_t<2, 2>>::value,
              "Test is_same_length test");

static_assert(is_same_length<exponents_t<1, 1, 1>, exponents_t<2, 2, 2>>::value,
              "Test is_same_length test");

static_assert(!is_same_length<exponents_t<>, exponents_t<2>>::value,
              "Test is_same_length test");

static_assert(!is_same_length<exponents_t<1>, exponents_t<>>::value,
              "Test is_same_length test");

static_assert(!is_same_length<exponents_t<>, exponents_t<2, 2>>::value,
              "Test is_same_length test");

static_assert(!is_same_length<exponents_t<1, 1>, exponents_t<>>::value,
              "Test is_same_length test");

static_assert(!is_same_length<exponents_t<>, exponents_t<2, 2, 2>>::value,
              "Test is_same_length test");

static_assert(!is_same_length<exponents_t<1>, exponents_t<2, 2>>::value,
              "Test is_same_length test");

static_assert(!is_same_length<exponents_t<1, 1>, exponents_t<2>>::value,
              "Test is_same_length test");

static_assert(!is_same_length<exponents_t<1, 1, 1>, exponents_t<>>::value,
              "Test is_same_length test");

/*
 * exponents_zero_t, exponents_invert_t
 */

static_assert(is_same<exponents_t<0, 0, 0>,
                      exponents_zero_t<exponents_t<1, 2, 3>>>::value,
              "Test unary zerofy operation");

static_assert(is_same<exponents_t<-1, -2, -3>,
                      exponents_negate_t<exponents_t<1, 2, 3>>>::value,
              "Test unary negate operation");

/*
 * exponents_add_t, exponents_sub_t
 */

static_assert(is_same<exponents_add_t<exponents_t<1, 2, 3>, exponents_t<4, 5, 6>>,
                      exponents_t<5, 7, 9>>::value,
              "Test addition");

static_assert(is_same<exponents_sub_t<exponents_t<1, 2, 3>, exponents_t<4, 5, 6>>,
                      exponents_t<-3, -3, -3>>::value,
              "Test subtraction");

/*
 * exponents_mod_t
 */

template <class Exponents, sint Divisor, class = void>
struct test_exponents_mod : false_type {};

template <class Exponents, sint Divisor>
struct test_exponents_mod<Exponents, Divisor,
                          void_t<exponents_mod_t<Exponents, Divisor>>>
    : true_type {};

static_assert(test_exponents_mod<exponents_t<1, 2, 3>, 1>::value,
              "Taking exponents % 1 should be possible");

static_assert(!test_exponents_mod<exponents_t<1, 2, 3>, 0>::value,
              "Taking exponents % 0 should fail");

static_assert(is_same<exponents_mod_t<exponents_t<1, 2, 3>, 1>,
                      exponents_t<0, 0, 0>>::value,
              "Test exponents % 1");

static_assert(is_same<exponents_mod_t<exponents_t<1, 2, 3>, 2>,
                      exponents_t<1, 0, 1>>::value,
              "Test exponents % 2");

static_assert(is_same<exponents_mod_t<exponents_t<1, 2, 3>, 3>,
                      exponents_t<1, 2, 0>>::value,
              "Test exponents % 3");

static_assert(is_same<exponents_mod_t<exponents_t<1, 2, 3>, -1>,
                      exponents_t<0, 0, 0>>::value,
              "Test exponents % -1");

static_assert(is_same<exponents_mod_t<exponents_t<1, 2, 3>, -2>,
                      exponents_t<1, 0, 1>>::value,
              "Test exponents % -2");

static_assert(is_same<exponents_mod_t<exponents_t<1, 2, 3>, -3>,
                      exponents_t<1, 2, 0>>::value,
              "Test exponents % -3");

/*
 * exponents_div_t
 */

template <class Exponents, sint Divisor, class = void>
struct test_exponents_div : false_type {};

template <class Exponents, sint Divisor>
struct test_exponents_div<Exponents, Divisor,
                          void_t<exponents_div_t<Exponents, Divisor>>>
    : true_type {};

static_assert(test_exponents_div<exponents_t<1, 2, 3>, 1>::value,
              "Taking exponents / 1 should be possible");

static_assert(!test_exponents_div<exponents_t<1, 2, 3>, 0>::value,
              "Taking exponents / 0 should fail");

static_assert(is_same<exponents_div_t<exponents_t<1, 2, 3>, 1>,
                      exponents_t<1, 2, 3>>::value,
              "Test exponents / 1");

static_assert(exponents_div<exponents_t<1, 2, 3>, 1>::exact,
              "Test exponents / 1 should be exact");

static_assert(is_same<exponents_div_t<exponents_t<1, 2, 3>, 2>,
                      exponents_t<0, 1, 1>>::value,
              "Test exponents / 2");

static_assert(!exponents_div<exponents_t<1, 2, 3>, 2>::exact,
              "Test exponents / 2 should not be exact");

static_assert(is_same<exponents_div_t<exponents_t<1, 2, 3>, 3>,
                      exponents_t<0, 0, 1>>::value,
              "Test exponents / 3");

static_assert(!exponents_div<exponents_t<1, 2, 3>, 3>::exact,
              "Test exponents / 3 should not be exact");

static_assert(is_same<exponents_div_t<exponents_t<1, 2, 3>, -1>,
                      exponents_t<-1, -2, -3>>::value,
              "Test exponents / -1");

static_assert(exponents_div<exponents_t<1, 2, 3>, -1>::exact,
              "Test exponents / -1 should be exact");

static_assert(is_same<exponents_div_t<exponents_t<1, 2, 3>, -2>,
                      exponents_t<0, -1, -1>>::value,
              "Test exponents / -2");

static_assert(!exponents_div<exponents_t<1, 2, 3>, -2>::exact,
              "Test exponents / -2 should not be exact");

static_assert(is_same<exponents_div_t<exponents_t<1, 2, 3>, -3>,
                      exponents_t<0, 0, -1>>::value,
              "Test exponents / -3");

static_assert(!exponents_div<exponents_t<1, 2, 3>, -3>::exact,
              "Test exponents / -3 should not be exact");

static_assert(is_same<exponents_div_t<exponents_t<0, 2, -4>, 2>,
                      exponents_t<0, 1, -2>>::value,
              "Test exponents <0, 2, -4> / 2");

static_assert(exponents_div<exponents_t<0, 2, -4>, 2>::exact,
              "Test exponents <0, 2, -4> / 2 should be exact");

/*
 * is_constant
 */

struct static_constexpr_double_const {
	static constexpr double const value{3.14159265358979323846};
};

static_assert(is_constant<static_constexpr_double_const>::value,
              "Test check for is_constant");

struct static_constexpr_double {
	static constexpr double value{3.14159265358979323846};
};

static_assert(is_constant<static_constexpr_double>::value,
              "Test check for is_constant");

struct static_double {
	static double value;
};

static_assert(!is_constant<static_double>::value,
              "Test check for is_constant, non-constexpr value");

struct static_constexpr_rational_const {
	static constexpr rational<7, 8> const value{};
};

static_assert(is_constant<static_constexpr_rational_const>::value,
              "Test check for is_constant, rational is convertible");

struct static_constexpr_ptr_const {
	static constexpr char const * const value{"foobar"};
};

static_assert(!is_constant<static_constexpr_ptr_const>::value,
              "Test check for is_constant, pointer is not convertible");

static_assert(!is_constant<double>::value,
              "Test check for is_constant, double is not a container");

/*
 * constants_chain, is_constants_chain, constants_t
 */

struct constant_Pi {
	static constexpr double const value{3.14159265358979323846};
};

static_assert(is_constants_chain<void>::value,
              "Test is_constants_chain check");

static_assert(is_constants_chain<constants_chain<constant_Pi>>::value,
              "Test is_constants_chain check");

static_assert(is_constants_chain<constants_chain<constant_Pi, constants_chain<constant_Pi>>>::value,
              "Test is_constants_chain check");

static_assert(!is_constants_chain<double>::value,
              "Test is_constants_chain check");

static_assert(!is_constants_chain<exponents_t<1, 2, 3>>::value,
              "Test is_constants_chain check");

static_assert(is_same<void, constants_t<>>::value,
              "Test whether constants_t creates the correct type");

static_assert(is_same<constants_chain<constant_Pi>, constants_t<constant_Pi>>::value,
              "Test whether constants_t creates the correct type");

static_assert(is_same<constants_chain<constant_Pi, constants_chain<constant_Pi>>,
                      constants_t<constant_Pi, constant_Pi>>::value,
              "Test whether constants_t creates the correct type");

static_assert(is_same<constants_chain<constant_Pi, constants_chain<constant_Pi, constants_chain<constant_Pi>>>,
                      constants_t<constant_Pi, constant_Pi, constant_Pi>>::value,
              "Test whether constants_t creates the correct type");

struct fake_constants_chain {
	typedef void type;
	typedef void tail;
};

static_assert(!is_constants_chain<fake_constants_chain>::value,
              "The fake_constants_chain type is not a constants_chain instance");

static_assert(!is_constants_chain<constants_chain<constant_Pi, fake_constants_chain>>::value,
              "The fake_constants_chain type is not a constants_chain instance");

struct fail_constants_chain {
};

static_assert(!is_constants_chain<fail_constants_chain>::value,
              "The fail_constants_chain type is not a constants_chain instance");

static_assert(!is_constants_chain<constants_chain<constant_Pi, fail_constants_chain>>::value,
              "The fail_constants_chain type is not a constants_chain instance");

struct constant_A {
	static constexpr double const value{2.};
};

struct constant_B {
	static constexpr double const value{10.};
};

struct constant_C {
	static constexpr double const value{1.41421356237309504880};
};

typedef constants_t<constant_A, constant_B, constant_C> consts_ABC;

static_assert(consts_ABC::type::value == 2.,
              "Test constants_chain");

static_assert(consts_ABC::tail::type::value == 10.,
              "Test constants_chain");

static_assert(consts_ABC::tail::tail::type::value == 1.41421356237309504880,
              "Test constants_chain");

/*
 * constant_pow
 */

static_assert(constant_pow<double, constant_Pi, 0>::value == 1.,
              "Test constant to the power 0");

static_assert(constant_pow<double, constant_Pi, -1>::value ==
              1. / constant_Pi::value,
              "Test constant to the power -1");

static_assert(constant_pow<double, constant_Pi, 2>::value == 
              constant_Pi::value * constant_Pi::value,
              "Test constant to the power 2");

static_assert(constant_pow<double, constant_Pi, -2>::value ==
              1. / (constant_Pi::value * constant_Pi::value),
              "Test constant to the power -2");

/*
 * constants_pack, constants_pack_t
 */

static_assert(is_constants_pack<constants_pack<consts_ABC, exponents_t<1, 0, 0>>>::value,
              "Test is_constants_pack test");

static_assert(!is_constants_pack<void>::value,
              "Test is_constants_pack test");

static_assert(!is_constants_pack<double>::value,
              "Test is_constants_pack test");

static_assert(is_same<constants_pack<consts_ABC, exponents_t<2, 1, 0>>,
                      constants_pack_t<consts_ABC, 2, 1, 0>>::value,
              "Test constants_pack");

static_assert(is_same<constants_pack_t<consts_ABC, 2, 1, 0>::constants,
                      consts_ABC>::value,
              "Test constants_pack");

static_assert(is_same<constants_pack_t<consts_ABC, 2, 1, 0>::exponents,
                      exponents_t<2, 1, 0>>::value,
              "Test constants_pack");

/*
 * constants_pack_neutral_t, constants_pack_invert_t
 */

static_assert(is_same<constants_pack_neutral_t<constants_pack_t<consts_ABC, 2, 1, 0>>,
                      constants_pack_t<consts_ABC, 0, 0, 0>>::value,
              "Test constants_pack_neutral_t");

static_assert(is_same<constants_pack_invert_t<constants_pack_t<consts_ABC, 2, 1, 0>>,
                      constants_pack_t<consts_ABC, -2, -1, 0>>::value,
              "Test constants_pack_invert_t");

/*
 * constants_pack_mul_t, constants_pack_div_t
 */

static_assert(is_same<constants_pack_mul_t<constants_pack_t<consts_ABC,  2,  1,  0>,
                                           constants_pack_t<consts_ABC, -1, -2, -3>>,
                      constants_pack_t<consts_ABC, 1, -1, -3>>::value,
              "Test constants_pack_mul_t");

static_assert(is_same<constants_pack_div_t<constants_pack_t<consts_ABC, 2, 1, 0>,
                                           constants_pack_t<consts_ABC, 1, 2, 3>>,
                      constants_pack_t<consts_ABC, 1, -1, -3>>::value,
              "Test constants_pack_div_t");

/*
 * constants_prod, constants_pack_prod
 */

static_assert(constants_prod<double, consts_ABC, exponents_t<3, 2, 1>>::value ==
              constants_pack_prod<double, constants_pack_t<consts_ABC, 3, 2, 1>>::value,
              "Test constants_prod");

static_assert(constants_pack_prod<double, constants_pack_t<consts_ABC, 2, 2, 0>>::value == 400.,
              "Test constants_prod");

static_assert(constants_pack_prod<double, constants_pack_t<consts_ABC, -3, 3, 0>>::value == 125.,
              "Test constants_prod");

/*
 * unit
 */

/* Define a set of units */

template <sint ... Exps>
using cnst = constants_pack_t<constants_t<constant_Pi>, Exps ...>;

typedef unit<double, exponents_t<0, 0, 0, 0>, rational<1, 1>, cnst<0>> scalar;

typedef unit<double, exponents_t<1, 0, 0, 0>, rational<1, 1>, cnst<0>> metre;

typedef unit<double, exponents_t<0, 1, 0, 0>, rational<1, 1>, cnst<0>> kilogram;

typedef unit<double, exponents_t<0, 0, 1, 0>, rational<1, 1>, cnst<0>> second;

typedef unit<double, exponents_t<0, 0, 0, 0>, rational<1000, 1>, cnst<0>> kilo;

typedef unit<double, exponents_t<0, 0, 0, 1>, rational<1, 1>, cnst<0>> rad;

typedef unit<double, exponents_t<1, 0, 0, 0>, rational<1000, 1>, cnst<0>> kilometre;

typedef unit<double, exponents_t<0, 0, 1, 0>, rational<3600, 1>, cnst<0>> hour;

typedef unit<double, exponents_t<0, 0, 0, 1>, rational<1, 180>, cnst<1>> deg;

/* Define incompatible units */

typedef unit<int, exponents_t<0, 0, 0, 0>, rational<1, 1>, cnst<0>> int_scalar;

typedef unit<double, exponents_t<0, 0, 0, 0>, rational<1, 1>> noconst_scalar;

typedef unit<double, exponents_t<0, 0, 0>,
                     rational<1, 1>,
                     cnst<0>> dim3_scalar;

/* Test is_unit */

static_assert(is_unit<scalar>::value,
              "Unit scalar is a unit");

static_assert(is_unit<int_scalar>::value,
              "Unit int_scalar is a unit");

static_assert(is_unit<metre>::value,
              "Unit metre is a unit");

static_assert(!is_unit<int>::value,
              "Type int is not a unit");

/* Test unit_unary, unit_scalar_t, unit_invert_t, unit_base_t */

static_assert(is_same<unit_scalar_t<scalar>, scalar>::value,
              "Test unit_scalar_t with unit scalar");

static_assert(is_same<unit_scalar_t<metre>, scalar>::value,
              "Test unit_scalar_t with unit metre");

static_assert(is_same<unit_scalar_t<deg>, scalar>::value,
              "Test unit_scalar_t with unit deg");

static_assert(is_same<unit_invert_t<scalar>, scalar>::value,
              "Test unit_invert_t with unit scalar");

static_assert(is_same<unit_invert_t<metre>,
                      unit<double, exponents_t<-1, 0, 0, 0>, rational<1, 1>, cnst<0>>>::value,
              "Test unit_invert_t with unit metre");

static_assert(is_same<unit_invert_t<deg>,
                      unit<double, exponents_t<0, 0, 0, -1>, rational<180, 1>, cnst<-1>>>::value,
              "Test unit_invert_t with unit metre");

static_assert(is_same<unit_base_t<scalar>, scalar>::value,
              "Test unit_base_t with unit scalar");

static_assert(is_same<unit_base_t<kilometre>, metre>::value,
              "Test unit_base_t with unit kilometre");

static_assert(is_same<unit_base_t<deg>, rad>::value,
              "Test unit_base_t with unit deg");

static_assert(is_same<unit_base_t<kilogram>, kilogram>::value,
              "Test unit_base_t with unit kilogram");

/* Test is_compatible_unit */

static_assert(is_compatible_unit<scalar, scalar>::value,
              "Unit pair scalar/scalar is compatible");

static_assert(!is_compatible_unit<scalar, int_scalar>::value,
              "Unit pair scalar/int_scalar is not compatible");

static_assert(is_compatible_unit<scalar, metre>::value,
              "Unit pair scalar/metre is compatible");

static_assert(!is_compatible_unit<scalar, int>::value,
              "Type pair scalar/int is not compatible");

static_assert(!is_compatible_unit<int, scalar>::value,
              "Type pair int/scalar is not compatible");

static_assert(!is_compatible_unit<int, int>::value,
              "Type pair int/int is not compatible");

/* Test unit_binary, unit_mul_t, unit_div_t */

static_assert(is_same<unit_mul_t<deg, hour>,
                      unit<double, exponents_t<0, 0, 1, 1>, rational<3600, 180>, cnst<1>>>::value,
              "Test multiplying types deg and hour");

static_assert(is_same<unit_div_t<deg, hour>,
                      unit<double, exponents_t<0, 0, -1, 1>, rational<1, 180 * 3600>, cnst<1>>>::value,
              "Test dividing types deg and hour");

/* Test unit_pow, unit_pow_t */

static_assert(is_same<unit_pow_t<deg, 0>, scalar>::value,
              "Test taking a unit to the power 0");

static_assert(is_same<unit_pow_t<deg, 1>, deg>::value,
              "Test taking a unit to the power 1");

static_assert(is_same<unit_pow_t<deg, 2>,
                      unit<double, exponents_t<0, 0, 0, 2>, rational<1, 32400>, cnst<2>>>::value,
              "Test taking a unit to the power 2");

static_assert(is_same<unit_pow_t<deg, 3>,
                      unit<double, exponents_t<0, 0, 0, 3>, rational<1, 5832000>, cnst<3>>>::value,
              "Test taking a unit to the power 2");

static_assert(is_same<unit_pow_t<deg, -1>, unit_invert_t<deg>>::value,
              "Test taking a unit to the power -1");

static_assert(is_same<unit_pow_t<deg, -2>, unit_invert_t<unit_pow_t<deg, 2>>>::value,
              "Test taking a unit to the power -2");

static_assert(is_same<unit_pow_t<deg, -3>, unit_invert_t<unit_pow_t<deg, 3>>>::value,
              "Test taking a unit to the power -3");

/* Test unit_pow::pow() */

static_assert(unit_pow<deg, 0>::pow(deg{2.}).value == 1.,
              "Test taking a unit to the power 0");

static_assert(unit_pow<deg, 1>::pow(deg{2.}).value == 2.,
              "Test taking a unit to the power 1");

static_assert(unit_pow<deg, 2>::pow(deg{2.}).value == 4.,
              "Test taking a unit to the power 2");

static_assert(unit_pow<deg, 3>::pow(deg{2.}).value == 8.,
              "Test taking a unit to the power 3");

static_assert(unit_pow<deg, -1>::pow(deg{2.}).value == .5,
              "Test taking a unit to the power -1");

static_assert(unit_pow<deg, -2>::pow(deg{2.}).value == .25,
              "Test taking a unit to the power -2");

static_assert(unit_pow<deg, -3>::pow(deg{2.}).value == .125,
              "Test taking a unit to the power -3");

/* Test unit_root, unit_root_t */

template <class Unit, sint Degree, class = void>
struct test_unit_root : false_type {};

template <class Unit, sint Degree>
struct test_unit_root<Unit, Degree, void_t<unit_root<Unit, Degree>>>
    : true_type {};

static_assert(test_unit_root<scalar, 1>::value,
              "The 1st root of any unit should be valid");

static_assert(!test_unit_root<scalar, 0>::value,
              "The 0th root of any unit should be invalid");

static_assert(test_unit_root<scalar, 2>::value,
              "Any root (except 0) of the scalar value should be valid");

static_assert(test_unit_root<scalar, -2>::value,
              "Any root (except 0) of the scalar value should be valid");

static_assert(!test_unit_root<deg, 2>::value,
              "The 2nd root of deg should be invalid");

static_assert(is_same<unit_root_t<deg, 1>, rad>::value,
              "Test taking the 0th root of a unit");

static_assert(is_same<unit_root_t<unit_pow_t<deg, -1>, -1>, rad>::value,
              "Test taking the 2nd root of a unit squared");

static_assert(is_same<unit_root_t<unit_pow_t<deg, 2>, 2>, rad>::value,
              "Test taking the 2nd root of a unit squared");

static_assert(is_same<unit_root_t<unit_pow_t<deg, -2>, -2>, rad>::value,
              "Test taking the 2nd root of a unit squared");

/* Test is_unit_scalar */

static_assert(is_unit_scalar<scalar>::value,
              "Unit scalar is a scalar unit");

static_assert(is_unit_scalar<int_scalar>::value,
              "Unit int_scalar is a scalar unit");

static_assert(!is_unit_scalar<metre>::value,
              "Unit metre is not a scalar unit");

static_assert(!is_unit_scalar<int>::value,
              "Type int is not a scalar unit");

static_assert(!is_unit_scalar<decltype(metre{} / kilometre{})>::value,
              "Unit 1/1000 is not a scalar unit");

static_assert(!is_unit_scalar<decltype(deg{} / rad{})>::value,
              "Unit 1/180 * PI is not a scalar unit");

/* Test has_unit_base_scalar */

static_assert(has_unit_base_scalar<scalar>::value,
              "Unit scalar is a scalar unit");

static_assert(has_unit_base_scalar<int_scalar>::value,
              "Unit int_scalar is a scalar unit");

static_assert(!has_unit_base_scalar<metre>::value,
              "Unit metre is not a scalar unit");

static_assert(!has_unit_base_scalar<int>::value,
              "Type int is not a scalar unit");

static_assert(has_unit_base_scalar<decltype(metre{} / kilometre{})>::value,
              "Unit 1/1000 is convertible to a scalar unit");

static_assert(has_unit_base_scalar<decltype(deg{} / rad{})>::value,
              "Unit 1/180 * PI is convertible to a scalar unit");

/* Test mixing of with incompatible unit int_scalar  */

static_assert(tests::construct_default<int_scalar>::value,
              "The unit should be constructible");

static_assert(!tests::construct_copy<int_scalar, scalar>::value,
              "Construction from incompatible unit set should fail to compile");

static_assert(!tests::construct_copy<scalar, int_scalar>::value,
              "Construction from incompatible unit set should fail to compile");

static_assert(!tests::op_assign<int_scalar, scalar>::value,
              "Assigning from incompatible unit set should fail to compile");

static_assert(!tests::op_assign<scalar, int_scalar>::value,
              "Assigning from incompatible unit set should fail to compile");

static_assert(!tests::op_plus<int_scalar, scalar>::value,
              "Addition with incompatible unit set should fail to compile");

static_assert(!tests::op_plus<scalar, int_scalar>::value,
              "Addition with incompatible unit set should fail to compile");

static_assert(!tests::op_minus<int_scalar, scalar>::value,
              "Subtraction with incompatible unit set should fail to compile");

static_assert(!tests::op_minus<scalar, int_scalar>::value,
              "Subtraction with incompatible unit set should fail to compile");

static_assert(!tests::op_multiply<int_scalar, scalar>::value,
              "Multiplication with incompatible unit set should fail to compile");

static_assert(!tests::op_multiply<scalar, int_scalar>::value,
              "Multiplication with incompatible unit set should fail to compile");

static_assert(!tests::op_divide<int_scalar, scalar>::value,
              "Division with incompatible unit set should fail to compile");

static_assert(!tests::op_divide<scalar, int_scalar>::value,
              "Division with incompatible unit set should fail to compile");

static_assert(!tests::op_modulo<int_scalar, scalar>::value,
              "Modulo with incompatible unit set should fail to compile");

static_assert(!tests::op_modulo<scalar, int_scalar>::value,
              "Modulo with incompatible unit set should fail to compile");

/* Test mixing of with incompatible unit noconst_scalar  */

static_assert(tests::construct_default<noconst_scalar>::value,
              "The unit should be constructible");

static_assert(!tests::construct_copy<noconst_scalar, scalar>::value,
              "Construction from incompatible unit set should fail to compile");

static_assert(!tests::construct_copy<scalar, noconst_scalar>::value,
              "Construction from incompatible unit set should fail to compile");

static_assert(!tests::op_assign<noconst_scalar, scalar>::value,
              "Assigning from incompatible unit set should fail to compile");

static_assert(!tests::op_assign<scalar, noconst_scalar>::value,
              "Assigning from incompatible unit set should fail to compile");

static_assert(!tests::op_plus<noconst_scalar, scalar>::value,
              "Addition with incompatible unit set should fail to compile");

static_assert(!tests::op_plus<scalar, noconst_scalar>::value,
              "Addition with incompatible unit set should fail to compile");

static_assert(!tests::op_minus<noconst_scalar, scalar>::value,
              "Subtraction with incompatible unit set should fail to compile");

static_assert(!tests::op_minus<scalar, noconst_scalar>::value,
              "Subtraction with incompatible unit set should fail to compile");

static_assert(!tests::op_multiply<noconst_scalar, scalar>::value,
              "Multiplication with incompatible unit set should fail to compile");

static_assert(!tests::op_multiply<scalar, noconst_scalar>::value,
              "Multiplication with incompatible unit set should fail to compile");

static_assert(!tests::op_divide<noconst_scalar, scalar>::value,
              "Division with incompatible unit set should fail to compile");

static_assert(!tests::op_divide<scalar, noconst_scalar>::value,
              "Division with incompatible unit set should fail to compile");

static_assert(!tests::op_modulo<noconst_scalar, scalar>::value,
              "Modulo with incompatible unit set should fail to compile");

static_assert(!tests::op_modulo<scalar, noconst_scalar>::value,
              "Modulo with incompatible unit set should fail to compile");

/* Test mixing of with incompatible unit dim3_scalar  */

static_assert(tests::construct_default<dim3_scalar>::value,
              "The unit should be constructible");

static_assert(!tests::construct_copy<dim3_scalar, scalar>::value,
              "Construction from incompatible unit set should fail to compile");

static_assert(!tests::construct_copy<scalar, dim3_scalar>::value,
              "Construction from incompatible unit set should fail to compile");

static_assert(!tests::op_assign<dim3_scalar, scalar>::value,
              "Assigning from incompatible unit set should fail to compile");

static_assert(!tests::op_assign<scalar, dim3_scalar>::value,
              "Assigning from incompatible unit set should fail to compile");

static_assert(!tests::op_plus<dim3_scalar, scalar>::value,
              "Addition with incompatible unit set should fail to compile");

static_assert(!tests::op_plus<scalar, dim3_scalar>::value,
              "Addition with incompatible unit set should fail to compile");

static_assert(!tests::op_minus<dim3_scalar, scalar>::value,
              "Subtraction with incompatible unit set should fail to compile");

static_assert(!tests::op_minus<scalar, dim3_scalar>::value,
              "Subtraction with incompatible unit set should fail to compile");

static_assert(!tests::op_multiply<dim3_scalar, scalar>::value,
              "Multiplication with incompatible unit set should fail to compile");

static_assert(!tests::op_multiply<scalar, dim3_scalar>::value,
              "Multiplication with incompatible unit set should fail to compile");

static_assert(!tests::op_divide<dim3_scalar, scalar>::value,
              "Division with incompatible unit set should fail to compile");

static_assert(!tests::op_divide<scalar, dim3_scalar>::value,
              "Division with incompatible unit set should fail to compile");

static_assert(!tests::op_modulo<dim3_scalar, scalar>::value,
              "Modulo with incompatible unit set should fail to compile");

static_assert(!tests::op_modulo<scalar, dim3_scalar>::value,
              "Modulo with incompatible unit set should fail to compile");

/* Test constructors */

static_assert(kilometre{}.value == 0.,
              "Test unit default construction");

static_assert(kilometre{2.}.value == 2.,
              "Test unit construction from scalar");

static_assert(kilometre{kilometre{3.}}.value == 3.,
              "Test copy/move construction");

static_assert(kilometre{tests::identity(kilometre{3.})}.value == 3.,
              "Test copy construction");

static_assert(kilometre{metre{2000.}}.value == 2.,
              "Test unit copy/move and conversion construction");

static_assert(kilometre{tests::identity(metre{2000.})}.value == 2.,
              "Test unit copy and conversion construction");

static_assert(rad{deg{180.}}.value == constant_Pi::value,
              "Test unit copy and conversion construction with constant");

static_assert(rad{tests::identity(deg{180.})}.value == constant_Pi::value,
              "Test unit copy/move and conversion construction with constant");

static_assert(deg{rad{constant_Pi::value}}.value == 180.,
              "Test unit copy/move and conversion construction with constant");

static_assert(deg{tests::identity(rad{constant_Pi::value})}.value == 180.,
              "Test unit copy and conversion construction with constant");

static_assert(tests::construct_copy<kilometre, kilometre>::value,
              "Verify that test recognises valid case");

static_assert(tests::construct_copy<kilometre, metre>::value,
              "Verify that test recognises valid case");

static_assert(!tests::construct_copy<kilometre, second>::value,
              "Copy and conversion with different base units should fail");

/* Test math operators with base types, i.e. conversion factor 1 */

static_assert(is_same<decltype(metre{10.} * second{2.}),
                      unit<double, exponents_t<1, 0, 1, 0>,
                                   rational<1, 1>,
                                   cnst<0>>>::value,
              "Test unit * unit type");

static_assert((metre{10.} * second{2.}).value == 20.,
              "Test unit * unit value");

static_assert(is_same<decltype(metre{10.} / second{2.}),
                      unit<double, exponents_t<1, 0, -1, 0>,
                                   rational<1, 1>,
                                   cnst<0>>>::value,
              "Test unit / unit type");

static_assert((metre{10.} / second{2.}).value == 5.,
              "Test unit / unit value");

static_assert(is_same<decltype(metre{10.} + metre{2.}),
                      metre>::value,
              "Test unit + unit type");

static_assert((metre{10.} + metre{2.}).value == 12.,
              "Test unit + unit value");

static_assert(is_same<decltype(metre{10.} - metre{2.}),
                      metre>::value,
              "Test unit - unit type");

static_assert((metre{10.} - metre{2.}).value == 8.,
              "Test unit - unit value");

/* Test math operators with non-base types, i.e. conversion factor != 1 */

static_assert(is_same<decltype(kilometre{10.} * hour{2.}),
                      unit<double, exponents_t<1   , 0,    1, 0>,
                                   rational<3600000, 1>,
                                   cnst<0>>>::value,
              "Test unit * unit type");

static_assert((kilometre{10.} * hour{2.}).value == 20.,
              "Test unit * unit value");

static_assert(is_same<decltype(deg{10.} * hour{2.}),
                      unit<double, exponents_t<0, 0,    1,   1>,
                                   rational<3600, 180>,
                                   cnst<1>>>::value,
              "Test unit * unit type");

static_assert((deg{10.} * hour{2.}).value == 20.,
              "Test unit * unit value");

static_assert(is_same<decltype(kilometre{10.} / hour{2.}),
                      unit<double, exponents_t<   1, 0,   -1, 0>,
                                   rational<1000, 3600>,
                                   cnst<0>>>::value,
              "Test unit / unit type");

static_assert((kilometre{10.} / hour{2.}).value == 5.,
              "Test unit / unit value");

static_assert(is_same<decltype(kilometre{10.} / deg{2.}),
                      unit<double, exponents_t<   1, 0, 0,  -1>,
                                   rational<180000, 1>,
                                   cnst<-1>>>::value,
              "Test unit / unit type");

static_assert((kilometre{10.} / deg{2.}).value == 5.,
              "Test unit / unit value");

static_assert(is_same<decltype(kilometre{10.} + metre{2.}),
                      kilometre>::value,
              "Test unit + unit type");

static_assert((kilometre{10.} + metre{2.}).value == 10.002,
              "Test unit + unit value");

static_assert(is_same<decltype(metre{10.} + kilometre{2.}),
                      metre>::value,
              "Test unit + unit type");

static_assert((metre{10.} + kilometre{2.}).value == 2010.,
              "Test unit + unit value");

static_assert(is_same<decltype(kilometre{10.} - metre{2.}),
                      kilometre>::value,
              "Test unit - unit type");

static_assert((kilometre{10.} - metre{2.}).value == 9.998,
              "Test unit - unit value");

static_assert(is_same<decltype(metre{10.} - kilometre{2.}),
                      metre>::value,
              "Test unit - unit type");

static_assert((metre{10.} - kilometre{2.}).value == -1990.,
              "Test unit - unit value");

/* Test math operators with incompatible types */

static_assert(!tests::op_plus<metre, double>::value,
              "Addition with primitive should fail to compile");

static_assert(!tests::op_plus<double, metre>::value,
              "Addition with primitive should fail to compile");

static_assert(!tests::op_plus<metre, second>::value,
              "Addition with different dimensions should fail to compile");

static_assert(!tests::op_plus<second, metre>::value,
              "Addition with different dimensions should fail to compile");

static_assert(!tests::op_plus<kilometre, hour>::value,
              "Addition with different dimensions should fail to compile");

static_assert(!tests::op_plus<hour, kilometre>::value,
              "Addition with different dimensions should fail to compile");

static_assert(!tests::op_plus<rad, metre>::value,
              "Addition with different dimensions should fail to compile");

static_assert(!tests::op_plus<metre, rad>::value,
              "Addition with different dimensions should fail to compile");

static_assert(!tests::op_minus<metre, second>::value,
              "Subtraction with different dimensions should fail to compile");

static_assert(!tests::op_minus<second, metre>::value,
              "Subtraction with different dimensions should fail to compile");

static_assert(!tests::op_minus<kilometre, hour>::value,
              "Subtraction with different dimensions should fail to compile");

static_assert(!tests::op_minus<hour, kilometre>::value,
              "Subtraction with different dimensions should fail to compile");

static_assert(!tests::op_minus<rad, metre>::value,
              "Subtraction with different dimensions should fail to compile");

static_assert(!tests::op_minus<metre, rad>::value,
              "Subtraction with different dimensions should fail to compile");

/* Test boolean operators with identical types */

static_assert(metre{10.} == metre{10.},
              "Test unit == unit");

static_assert(!(metre{10.} == metre{11.}),
              "Test unit == unit");

static_assert(metre{10.} != metre{5.},
              "Test unit != unit");

static_assert(!(metre{10.} != metre{10.}),
              "Test unit != unit");

static_assert(metre{5.} < metre{10.},
              "Test unit < unit");

static_assert(!(metre{10.} < metre{10.}),
              "Test unit < unit");

static_assert(metre{5.} <= metre{10.},
              "Test unit <= unit");

static_assert(metre{10.} <= metre{10.},
              "Test unit <= unit");

static_assert(!(metre{10.125} <= metre{10.}),
              "Test unit <= unit");

static_assert(metre{10.} > metre{5.},
              "Test unit > unit");

static_assert(!(metre{10.} > metre{10.}),
              "Test unit > unit");

static_assert(metre{10.} >= metre{5.},
              "Test unit >= unit");

static_assert(metre{10.} >= metre{10.},
              "Test unit >= unit");

static_assert(!(metre{10.} >= metre{10.125}),
              "Test unit >= unit");

/* Test == operator with type conversion */

static_assert(kilometre{10.} == metre{10000.},
              "Test unit == unit");

static_assert(!(kilometre{10.} == metre{10.}),
              "Test unit == unit");

static_assert(deg{180.} == rad{constant_Pi::value},
              "Test unit == unit");

static_assert(!(deg{181.} == rad{constant_Pi::value}),
              "Test unit == unit");

static_assert(metre{10000.} == kilometre{10.},
              "Test unit == unit");

static_assert(!(metre{10.} == kilometre{10.}),
              "Test unit == unit");

static_assert(rad{constant_Pi::value} == deg{180.},
              "Test unit == unit");

static_assert(!(rad{constant_Pi::value} == deg{181.}),
              "Test unit == unit");

/* Test == operator with incompatible types */

static_assert(!tests::op_equal<metre, double>::value,
              "Comparison with primitive should fail to compile");

static_assert(!tests::op_equal<double, metre>::value,
              "Comparison with primitive should fail to compile");

static_assert(!tests::op_equal<metre, scalar>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_equal<scalar, metre>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_equal<metre, second>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_equal<second, metre>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_equal<metre, rad>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_equal<rad, metre>::value,
              "Comparison with incompatible unit should fail to compile");

/* Test != operator with type conversion */

static_assert(kilometre{10.} != metre{10.},
              "Test unit != unit");

static_assert(!(kilometre{10.} != metre{10000.}),
              "Test unit != unit");

static_assert(rad{constant_Pi::value} != deg{181.},
              "Test unit != unit");

static_assert(!(rad{constant_Pi::value} != deg{180.}),
              "Test unit != unit");

static_assert(metre{10.} != kilometre{10.},
              "Test unit != unit");

static_assert(!(metre{10000.} != kilometre{10.}),
              "Test unit != unit");

static_assert(deg{181.} != rad{constant_Pi::value},
              "Test unit != unit");

static_assert(!(deg{180.} != rad{constant_Pi::value}),
              "Test unit != unit");

/* Test != operator with incompatible types */

static_assert(!tests::op_unequal<metre, double>::value,
              "Comparison with primitive should fail to compile");

static_assert(!tests::op_unequal<double, metre>::value,
              "Comparison with primitive should fail to compile");

static_assert(!tests::op_unequal<metre, scalar>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_unequal<scalar, metre>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_unequal<metre, second>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_unequal<second, metre>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_unequal<metre, rad>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_unequal<rad, metre>::value,
              "Comparison with incompatible unit should fail to compile");

/* Test > operator with type conversion */

static_assert(kilometre{10.} > metre{10.},
              "Test unit > unit");

static_assert(!(kilometre{10.} > metre{10000.}),
              "Test unit > unit");

static_assert(deg{181.} > rad{constant_Pi::value},
              "Test unit > unit");

static_assert(!(deg{180.} > rad{constant_Pi::value}),
              "Test unit > unit");

static_assert(metre{10001.} > kilometre{10.},
              "Test unit > unit");

static_assert(!(metre{10000.} > kilometre{10.}),
              "Test unit > unit");

static_assert(rad{constant_Pi::value} > deg{179.},
              "Test unit > unit");

static_assert(!(rad{constant_Pi::value} > deg{180.}),
              "Test unit > unit");

/* Test > operator with incompatible types */

static_assert(!tests::op_greater<metre, double>::value,
              "Comparison with primitive should fail to compile");

static_assert(!tests::op_greater<double, metre>::value,
              "Comparison with primitive should fail to compile");

static_assert(!tests::op_greater<metre, scalar>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_greater<scalar, metre>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_greater<metre, second>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_greater<second, metre>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_greater<metre, rad>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_greater<rad, metre>::value,
              "Comparison with incompatible unit should fail to compile");

/* Test >= operator with type conversion */

static_assert(kilometre{10.} >= metre{10.},
              "Test unit >= unit");

static_assert(kilometre{10.} >= metre{10000.},
              "Test unit >= unit");

static_assert(!(kilometre{10.} >= metre{10001.}),
              "Test unit >= unit");

static_assert(deg{180.} >= rad{constant_Pi::value},
              "Test unit >= unit");

static_assert(deg{181.} >= rad{constant_Pi::value},
              "Test unit >= unit");

static_assert(!(deg{179.} >= rad{constant_Pi::value}),
              "Test unit >= unit");

static_assert(metre{10000.} >= kilometre{9.},
              "Test unit >= unit");

static_assert(metre{10000.} >= kilometre{10.},
              "Test unit >= unit");

static_assert(!(metre{10000.} >= kilometre{11.}),
              "Test unit >= unit");

static_assert(rad{constant_Pi::value} >= deg{180.},
              "Test unit >= unit");

static_assert(rad{constant_Pi::value} >= deg{179.},
              "Test unit >= unit");

static_assert(!(rad{constant_Pi::value} >= deg{181.}),
              "Test unit >= unit");

/* Test >= operator with incompatible types */

static_assert(!tests::op_greater_equal<metre, double>::value,
              "Comparison with primitive should fail to compile");

static_assert(!tests::op_greater_equal<double, metre>::value,
              "Comparison with primitive should fail to compile");

static_assert(!tests::op_greater_equal<metre, scalar>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_greater_equal<scalar, metre>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_greater_equal<metre, second>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_greater_equal<second, metre>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_greater_equal<metre, rad>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_greater_equal<rad, metre>::value,
              "Comparison with incompatible unit should fail to compile");

/* Test < operator with type conversion */

static_assert(kilometre{10.} < metre{10001.},
              "Test unit < unit");

static_assert(!(kilometre{10.} < metre{10000.}),
              "Test unit < unit");

static_assert(deg{179.} < rad{constant_Pi::value},
              "Test unit < unit");

static_assert(!(deg{180.} < rad{constant_Pi::value}),
              "Test unit < unit");

static_assert(metre{9999.} < kilometre{10.},
              "Test unit < unit");

static_assert(!(metre{10000.} < kilometre{10.}),
              "Test unit < unit");

static_assert(rad{constant_Pi::value} < deg{181.},
              "Test unit < unit");

static_assert(!(rad{constant_Pi::value} < deg{180.}),
              "Test unit < unit");

/* Test < operator with incompatible types */

static_assert(!tests::op_less<metre, double>::value,
              "Comparison with primitive should fail to compile");

static_assert(!tests::op_less<double, metre>::value,
              "Comparison with primitive should fail to compile");

static_assert(!tests::op_less<metre, scalar>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_less<scalar, metre>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_less<metre, second>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_less<second, metre>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_less<metre, rad>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_less<rad, metre>::value,
              "Comparison with incompatible unit should fail to compile");

/* Test <= operator with type conversion */

static_assert(kilometre{10.} <= metre{10001.},
              "Test unit <= unit");

static_assert(kilometre{10.} <= metre{10000.},
              "Test unit <= unit");

static_assert(!(kilometre{10.} <= metre{9999.}),
              "Test unit <= unit");

static_assert(deg{180.} <= rad{constant_Pi::value},
              "Test unit <= unit");

static_assert(deg{179.} <= rad{constant_Pi::value},
              "Test unit <= unit");

static_assert(!(deg{181.} <= rad{constant_Pi::value}),
              "Test unit <= unit");

static_assert(metre{9999.} <= kilometre{10.},
              "Test unit <= unit");

static_assert(metre{10000.} <= kilometre{10.},
              "Test unit <= unit");

static_assert(!(metre{10001.} <= kilometre{10.}),
              "Test unit <= unit");

static_assert(rad{constant_Pi::value} <= deg{180.},
              "Test unit <= unit");

static_assert(rad{constant_Pi::value} <= deg{181.},
              "Test unit <= unit");

static_assert(!(rad{constant_Pi::value} <= deg{179.}),
              "Test unit <= unit");

/* Test <= operator with incompatible types */

static_assert(!tests::op_less_equal<metre, double>::value,
              "Comparison with primitive should fail to compile");

static_assert(!tests::op_less_equal<double, metre>::value,
              "Comparison with primitive should fail to compile");

static_assert(!tests::op_less_equal<metre, scalar>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_less_equal<scalar, metre>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_less_equal<metre, second>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_less_equal<second, metre>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_less_equal<metre, rad>::value,
              "Comparison with incompatible unit should fail to compile");

static_assert(!tests::op_less_equal<rad, metre>::value,
              "Comparison with incompatible unit should fail to compile");

/* Test operations with primitive values */

static_assert(metre{2.} * 3. == metre{6.},
              "Test unit * primitive");

static_assert(metre{3.} / 2. == metre{1.5},
              "Test unit / primitive");

static_assert(scalar{3.} + 2. == 5.,
              "Test scalar + primitive");

static_assert(scalar{3.} - 2. == 1.,
              "Test scalar - primitive");

static_assert(kilo{3.} + 125. == 3125.,
              "Test scalar convertible + primitive");

static_assert(kilo{3.} - 125. == 2875.,
              "Test scalar convertible - primitive");

static_assert(3. * metre{2.} == metre{6.},
              "Test primitive * unit");

static_assert(3. / metre{2.} == scalar{3.} / metre{2.},
              "Test primitive / unit");

static_assert(3. + scalar{2.} == scalar{5.},
              "Test primitive + scalar");

static_assert(3. - scalar{2.} == scalar{1.},
              "Test primitive - scalar");

static_assert(3. + kilo{2.} == scalar{2003.},
              "Test primitive + scalar convertible");

static_assert(3. - kilo{2.} == scalar{-1997.},
              "Test primitive - scalar convertible");

/* Test operations with primitive values that require implicit conversion */

static_assert(metre{2.} * 3 == metre{6.},
              "Test unit * primitive");

static_assert(metre{3.} / 2 == metre{1.5},
              "Test unit / primitive");

static_assert(scalar{3.} + 2 == 5.,
              "Test scalar + primitive");

static_assert(scalar{3.} - 2 == 1.,
              "Test scalar - primitive");

static_assert(kilo{3.} + 125 == 3125.,
              "Test scalar convertible + primitive");

static_assert(kilo{3.} - 125 == 2875.,
              "Test scalar convertible - primitive");

static_assert(3 * metre{2.} == metre{6.},
              "Test primitive * unit");

static_assert(3 / metre{2.} == scalar{3.} / metre{2.},
              "Test primitive / unit");

static_assert(3 + scalar{2.} == scalar{5.},
              "Test primitive + scalar");

static_assert(3 - scalar{2.} == scalar{1.},
              "Test primitive - scalar");

static_assert(3 + kilo{2.} == scalar{2003.},
              "Test primitive + scalar convertible");

static_assert(3 - kilo{2.} == scalar{-1997.},
              "Test primitive - scalar convertible");

/* Test boolean operators with primitive values */

static_assert(scalar{10.} == 10.,
              "Test scalar == primitive");

static_assert(!(scalar{10.} == 11.),
              "Test scalar == primitive");

static_assert(10. == scalar{10.},
              "Test primitive == scalar");

static_assert(!(11. == scalar{10.}),
              "Test primitive == scalar");

static_assert(scalar{10.} != 5.,
              "Test scalar != primitive");

static_assert(!(scalar{10.} != 10.),
              "Test scalar != primitive");

static_assert(5. != scalar{10.},
              "Test primitive != scalar");

static_assert(!(10. != scalar{10.}),
              "Test primitive != scalar");

static_assert(scalar{5.} < 10.,
              "Test scalar < primitive");

static_assert(!(scalar{10.} < 10.),
              "Test scalar < primitive");

static_assert(5. < scalar{10.},
              "Test primitive < scalar");

static_assert(!(10. < scalar{10.}),
              "Test primitive < scalar");

static_assert(scalar{5.} <= 10.,
              "Test scalar <= primitive");

static_assert(scalar{10.} <= 10.,
              "Test scalar <= primitive");

static_assert(!(scalar{10.125} <= 10.),
              "Test scalar <= primitive");

static_assert(5. <= scalar{10.},
              "Test primitive <= scalar");

static_assert(10. <= scalar{10.},
              "Test primitive <= scalar");

static_assert(!(10.125 <= scalar{10.}),
              "Test primitive <= scalar");

static_assert(scalar{10.} > 5.,
              "Test scalar > primitive");

static_assert(!(scalar{10.} > 10.),
              "Test scalar > primitive");

static_assert(10. > scalar{5.},
              "Test primitive > scalar");

static_assert(!(10. > scalar{10.}),
              "Test primitive > scalar");

static_assert(scalar{10.} >= 5.,
              "Test scalar >= primitive");

static_assert(scalar{10.} >= 10.,
              "Test scalar >= primitive");

static_assert(!(scalar{10.} >= 10.125),
              "Test scalar >= primitive");

static_assert(10. >= scalar{5.},
              "Test primitive >= scalar");

static_assert(10. >= scalar{10.},
              "Test primitive >= scalar");

static_assert(!(10. >= scalar{10.125}),
              "Test primitive >= scalar");

/* Test boolean operators with primitive values that require
 * implicit conversion */

static_assert(scalar{10.} == 10,
              "Test scalar == primitive");

static_assert(!(scalar{10.} == 11),
              "Test scalar == primitive");

static_assert(10 == scalar{10.},
              "Test primitive == scalar");

static_assert(!(11 == scalar{10.}),
              "Test primitive == scalar");

static_assert(scalar{10.} != 5,
              "Test scalar != primitive");

static_assert(!(scalar{10.} != 10),
              "Test scalar != primitive");

static_assert(5 != scalar{10.},
              "Test primitive != scalar");

static_assert(!(10 != scalar{10.}),
              "Test primitive != scalar");

static_assert(scalar{5.} < 10,
              "Test scalar < primitive");

static_assert(!(scalar{10.} < 10),
              "Test scalar < primitive");

static_assert(5 < scalar{10.},
              "Test primitive < scalar");

static_assert(!(10 < scalar{10.}),
              "Test primitive < scalar");

static_assert(scalar{5.} <= 10,
              "Test scalar <= primitive");

static_assert(scalar{10.} <= 10,
              "Test scalar <= primitive");

static_assert(!(scalar{10.125} <= 10),
              "Test scalar <= primitive");

static_assert(5 <= scalar{10.},
              "Test primitive <= scalar");

static_assert(10 <= scalar{10.},
              "Test primitive <= scalar");

static_assert(!(11 <= scalar{10.}),
              "Test primitive <= scalar");

static_assert(scalar{10.} > 5,
              "Test scalar > primitive");

static_assert(!(scalar{10.} > 10),
              "Test scalar > primitive");

static_assert(10 > scalar{5.},
              "Test primitive > scalar");

static_assert(!(10 > scalar{10.}),
              "Test primitive > scalar");

static_assert(scalar{10.} >= 5,
              "Test scalar >= primitive");

static_assert(scalar{10.} >= 10,
              "Test scalar >= primitive");

static_assert(!(scalar{10.} >= 11),
              "Test scalar >= primitive");

static_assert(10 >= scalar{5.},
              "Test primitive >= scalar");

static_assert(10 >= scalar{10.},
              "Test primitive >= scalar");

static_assert(!(10 >= scalar{10.125}),
              "Test primitive >= scalar");

/* Test = assignment */

static_assert(tests::op_assign<metre, metre>::value,
              "Assignment of identical units should compile");

static_assert(tests::op_assign<metre, kilometre>::value,
              "Assignment of units with identical dimensions should compile");

static_assert(tests::op_assign<kilometre, metre>::value,
              "Assignment of units with identical dimensions should compile");

static_assert(!tests::op_assign<metre, scalar>::value,
              "Assignment from scalar should fail to compile");

static_assert(!tests::op_assign<scalar, metre>::value,
              "Assignment to scalar should fail to compile");

static_assert(tests::op_assign<scalar, scalar>::value,
              "Assignment from and to scalar should compile");

static_assert(!tests::op_assign<metre, double>::value,
              "Assignment from primitive should fail to compile");

static_assert(!tests::op_assign<double, metre>::value,
              "Assignment to primitive should fail to compile");

static_assert(!tests::op_assign<second, metre>::value,
              "Assignment from different dimensions should fail to compile");

static_assert(!tests::op_assign<metre, second>::value,
              "Assignment from different dimensions should fail to compile");

static_assert(!tests::op_assign<rad, metre>::value,
              "Assignment from different dimensions should fail to compile");

static_assert(!tests::op_assign<metre, rad>::value,
              "Assignment from different dimensions should fail to compile");

/* Test += assignment */

static_assert(tests::op_assign_plus<metre, metre>::value,
              "Addition assignment of identical units should compile");

static_assert(tests::op_assign_plus<metre, kilometre>::value,
              "Addition assignment of units with identical dimensions should compile");

static_assert(tests::op_assign_plus<kilometre, metre>::value,
              "Addition assignment of units with identical dimensions should compile");

static_assert(!tests::op_assign_plus<metre, scalar>::value,
              "Addition assignment from scalar should fail to compile");

static_assert(!tests::op_assign_plus<scalar, metre>::value,
              "Addition assignment to scalar should fail to compile");

static_assert(tests::op_assign_plus<scalar, scalar>::value,
              "Addition assignment from and to scalar should compile");

static_assert(!tests::op_assign_plus<metre, double>::value,
              "Addition assignment from primitive should fail to compile");

static_assert(!tests::op_assign_plus<double, metre>::value,
              "Addition assignment to primitive should fail to compile");

static_assert(!tests::op_assign_plus<second, metre>::value,
              "Addition assignment from different dimensions should fail to compile");

static_assert(!tests::op_assign_plus<metre, second>::value,
              "Addition assignment from different dimensions should fail to compile");

static_assert(!tests::op_assign_plus<rad, metre>::value,
              "Addition assignment from different dimensions should fail to compile");

static_assert(!tests::op_assign_plus<metre, rad>::value,
              "Addition assignment from different dimensions should fail to compile");

/* Test -= assignment */

static_assert(tests::op_assign_minus<metre, metre>::value,
              "Subtraction assignment of identical units should compile");

static_assert(tests::op_assign_minus<metre, kilometre>::value,
              "Subtraction assignment of units with identical dimensions should compile");

static_assert(tests::op_assign_minus<kilometre, metre>::value,
              "Subtraction assignment of units with identical dimensions should compile");

static_assert(!tests::op_assign_minus<metre, scalar>::value,
              "Subtraction assignment from scalar should fail to compile");

static_assert(!tests::op_assign_minus<scalar, metre>::value,
              "Subtraction assignment to scalar should fail to compile");

static_assert(tests::op_assign_minus<scalar, scalar>::value,
              "Subtraction assignment from and to scalar should compile");

static_assert(!tests::op_assign_minus<metre, double>::value,
              "Subtraction assignment from primitive should fail to compile");

static_assert(!tests::op_assign_minus<double, metre>::value,
              "Subtraction assignment to primitive should fail to compile");

static_assert(!tests::op_assign_minus<second, metre>::value,
              "Subtraction assignment from different dimensions should fail to compile");

static_assert(!tests::op_assign_minus<metre, second>::value,
              "Subtraction assignment from different dimensions should fail to compile");

static_assert(!tests::op_assign_minus<rad, metre>::value,
              "Subtraction assignment from different dimensions should fail to compile");

static_assert(!tests::op_assign_minus<metre, rad>::value,
              "Subtraction assignment from different dimensions should fail to compile");

/* Test *= assignment */

static_assert(!tests::op_assign_multiply<metre, metre>::value,
              "Multiplication assignment of identical units should fail to compile");

static_assert(!tests::op_assign_multiply<metre, kilometre>::value,
              "Multiplication assignment of units with identical dimensions should fail to compile");

static_assert(!tests::op_assign_multiply<kilometre, metre>::value,
              "Multiplication assignment of units with identical dimensions should fail to compile");

static_assert(tests::op_assign_multiply<metre, scalar>::value,
              "Multiplication assignment from scalar should compile");

static_assert(!tests::op_assign_multiply<scalar, metre>::value,
              "Multiplication assignment to scalar should fail to compile");

static_assert(tests::op_assign_multiply<scalar, scalar>::value,
              "Multiplication assignment from and to scalar should compile");

static_assert(tests::op_assign_multiply<metre, double>::value,
              "Multiplication assignment from primitive should compile");

static_assert(!tests::op_assign_multiply<double, metre>::value,
              "Multiplication assignment to primitive should fail to compile");

static_assert(!tests::op_assign_multiply<second, metre>::value,
              "Multiplication assignment from different dimensions should fail to compile");

static_assert(!tests::op_assign_multiply<metre, second>::value,
              "Multiplication assignment from different dimensions should fail to compile");

static_assert(!tests::op_assign_multiply<rad, metre>::value,
              "Multiplication assignment from different dimensions should fail to compile");

static_assert(!tests::op_assign_multiply<metre, rad>::value,
              "Multiplication assignment from different dimensions should fail to compile");

/* Test /= assignment */

static_assert(!tests::op_assign_divide<metre, metre>::value,
              "Division assignment of identical units should fail to compile");

static_assert(!tests::op_assign_divide<metre, kilometre>::value,
              "Division assignment of units with identical dimensions should fail to compile");

static_assert(!tests::op_assign_divide<kilometre, metre>::value,
              "Division assignment of units with identical dimensions should fail to compile");

static_assert(tests::op_assign_divide<metre, scalar>::value,
              "Division assignment from scalar should compile");

static_assert(!tests::op_assign_divide<scalar, metre>::value,
              "Division assignment to scalar should fail to compile");

static_assert(tests::op_assign_divide<scalar, scalar>::value,
              "Division assignment from and to scalar should compile");

static_assert(tests::op_assign_divide<metre, double>::value,
              "Division assignment from primitive should compile");

static_assert(!tests::op_assign_divide<double, metre>::value,
              "Division assignment to primitive should fail to compile");

static_assert(!tests::op_assign_divide<second, metre>::value,
              "Division assignment from different dimensions should fail to compile");

static_assert(!tests::op_assign_divide<metre, second>::value,
              "Division assignment from different dimensions should fail to compile");

static_assert(!tests::op_assign_divide<rad, metre>::value,
              "Division assignment from different dimensions should fail to compile");

static_assert(!tests::op_assign_divide<metre, rad>::value,
              "Division assignment from different dimensions should fail to compile");

/* Test %= assignment */

static_assert(tests::op_assign_modulo<metre, metre>::value,
              "Modulo assignment of identical units should compile");

static_assert(tests::op_assign_modulo<metre, kilometre>::value,
              "Modulo assignment of units with identical dimensions should compile");

static_assert(tests::op_assign_modulo<kilometre, metre>::value,
              "Modulo assignment of units with identical dimensions should compile");

static_assert(!tests::op_assign_modulo<metre, scalar>::value,
              "Modulo assignment from scalar should fail to compile");

static_assert(!tests::op_assign_modulo<scalar, metre>::value,
              "Modulo assignment to scalar should fail to compile");

static_assert(tests::op_assign_modulo<scalar, scalar>::value,
              "Modulo assignment from and to scalar should compile");

static_assert(!tests::op_assign_modulo<metre, double>::value,
              "Modulo assignment from primitive should fail to compile");

static_assert(!tests::op_assign_modulo<double, metre>::value,
              "Modulo assignment to primitive should fail to compile");

static_assert(!tests::op_assign_modulo<second, metre>::value,
              "Modulo assignment from different dimensions should fail to compile");

static_assert(!tests::op_assign_modulo<metre, second>::value,
              "Modulo assignment from different dimensions should fail to compile");

static_assert(!tests::op_assign_modulo<rad, metre>::value,
              "Modulo assignment from different dimensions should fail to compile");

static_assert(!tests::op_assign_modulo<metre, rad>::value,
              "Modulo assignment from different dimensions should fail to compile");

/* Test % operator */

static_assert(int_scalar{3} % int_scalar{2} == int_scalar{1},
              "For encapsulated types that support %, the % operator should "
              "be a constexpr function");

static_assert(int_scalar{3} % 2 == 1,
              "For encapsulated types that support %, the % operator should "
              "be a constexpr function");

static_assert(3 % int_scalar{2} == 1,
              "For encapsulated types that support %, the % operator should "
              "be a constexpr function");

/* Test % operator with implicit conversion */

static_assert(int_scalar{3} % 2. == 1,
              "For encapsulated types that support %, the % operator should "
              "be a constexpr function");

static_assert(3. % int_scalar{2} == 1,
              "For encapsulated types that support %, the % operator should "
              "be a constexpr function");

/* Test pow<0>() */

static_assert(scalar{23.}.pow<0>() == 1.,
              "Any value to the power of 0 should result in a scalar value");

static_assert(metre{2.}.pow<0>() == 1.,
              "Any value to the power of 0 should result in a scalar value");

static_assert(deg{42.}.pow<0>() == 1.,
              "Any value to the power of 0 should result in a scalar value");

static_assert(kilometre{1337.}.pow<0>() == 1.,
              "Any value to the power of 0 should result in a scalar value");

static_assert((deg{16000. * 360.} / second{60.}).pow<0>() == 1.,
              "Any value to the power of 0 should result in a scalar value");

/* Test pow<1>() */

static_assert(scalar{23.}.pow<1>() == 23.,
              "Any value to the power of 1 should not change");

static_assert(metre{2.}.pow<1>() == metre{2.},
              "Any value to the power of 1 should not change");

static_assert(deg{42.}.pow<1>() == deg{42.},
              "Any value to the power of 1 should not change");

static_assert(kilometre{1337.}.pow<1>() == kilometre{1337.},
              "Any value to the power of 1 should not change");

static_assert((deg{16000. * 360.} / second{60.}).pow<1>() ==
                   (deg{16000. * 360.} / second{60.}),
              "Any value to the power of 1 should not change");

/* Test pow<2>() */

static_assert(scalar{23.}.pow<2>() == 23. * 23.,
              "Taking to an integer power is equivalent to multiplication");

static_assert(metre{2.}.pow<2>() == metre{2.} * metre{2.},
              "Taking to an integer power is equivalent to multiplication");

static_assert(deg{42.}.pow<2>() == deg{42.} * deg{42.},
              "Taking to an integer power is equivalent to multiplication");

static_assert(kilometre{1337.}.pow<2>() == kilometre{1337.} * kilometre{1337.},
              "Taking to an integer power is equivalent to multiplication");

static_assert((deg{16000. * 360.} / second{60.}).pow<2>() ==
                   (deg{16000. * 360.} / second{60.}) *
                   (deg{16000. * 360.} / second{60.}),
              "Taking to an integer power is equivalent to multiplication");

/* Test pow<3>() */

static_assert(scalar{23.}.pow<3>() == scalar{23.}.pow<2>() * 23.,
              "Taking to an integer power is equivalent to multiplication");

static_assert(metre{2.}.pow<3>() == metre{2.}.pow<2>() * metre{2.},
              "Taking to an integer power is equivalent to multiplication");

static_assert(deg{42.}.pow<3>() == deg{42.}.pow<2>() * deg{42.},
              "Taking to an integer power is equivalent to multiplication");

static_assert(kilometre{1337.}.pow<3>() == kilometre{1337.}.pow<2>() *
                  kilometre{1337.},
              "Taking to an integer power is equivalent to multiplication");

static_assert((deg{16000. * 360.} / second{60.}).pow<3>() ==
                   (deg{16000. * 360.} / second{60.}).pow<2>() *
                   (deg{16000. * 360.} / second{60.}),
              "Taking to an integer power is equivalent to multiplication");

/* Test pow<-1>() */

static_assert(scalar{23.}.pow<-1>() == 1./23.,
              "Values to the power of -1 are inverted");

static_assert(metre{2.}.pow<-1>() == 1./metre{2.},
              "Values to the power of -1 are inverted");

static_assert(deg{42.}.pow<-1>() == 1./deg{42.},
              "Values to the power of -1 are inverted");

static_assert(kilometre{1337.}.pow<-1>() == 1./kilometre{1337.},
              "Values to the power of -1 are inverted");

static_assert((deg{16000. * 360.} / second{60.}).pow<-1>() ==
                   1./(deg{16000. * 360.} / second{60.}),
              "Values to the power of -1 are inverted");

/* Test pow<-2>() */

static_assert(scalar{23.}.pow<-2>() == 1./scalar{23.}.pow<2>(),
              "Values to a negative power are inversions of the postive power");

static_assert(metre{2.}.pow<-2>() == 1./metre{2.}.pow<2>(),
              "Values to a negative power are inversions of the postive power");

static_assert(deg{42.}.pow<-2>() == 1./deg{42.}.pow<2>(),
              "Values to a negative power are inversions of the postive power");

static_assert(kilometre{1337.}.pow<-2>() == 1./kilometre{1337.}.pow<2>(),
              "Values to a negative power are inversions of the postive power");

static_assert((deg{16000. * 360.} / second{60.}).pow<-2>() ==
                   1./(deg{16000. * 360.} / second{60.}).pow<2>(),
              "Values to a negative power are inversions of the postive power");

/* Test pow<-3>() */

static_assert(scalar{23.}.pow<-3>() == 1./scalar{23.}.pow<3>(),
              "Values to a negative power are inversions of the postive power");

static_assert(metre{2.}.pow<-3>() == 1./metre{2.}.pow<3>(),
              "Values to a negative power are inversions of the postive power");

static_assert(deg{42.}.pow<-3>() == 1./deg{42.}.pow<3>(),
              "Values to a negative power are inversions of the postive power");

static_assert(kilometre{1337.}.pow<-3>() == 1./kilometre{1337.}.pow<3>(),
              "Values to a negative power are inversions of the postive power");

static_assert((deg{16000. * 360.} / second{60.}).pow<-3>() ==
                   1./(deg{16000. * 360.} / second{60.}).pow<3>(),
              "Values to a negative power are inversions of the postive power");

/* Test root<N>() */

template <class Unit, sint Root, class = void>
struct test_has_root : false_type {};

template <class Unit, sint Root>
struct test_has_root<Unit, Root,
                     void_t<decltype(Unit{}.template root<Root>())>>
    : true_type {};

static_assert(!test_has_root<scalar, 0>::value,
              "The 0th root should never work");

static_assert(!test_has_root<metre, 0>::value,
              "The 0th root should never work");

static_assert(test_has_root<metre, 1>::value,
              "The 1st root should always work");

static_assert(is_same<decltype(metre{}.root<1>()), metre>::value,
              "The 1st root should always return the base type");

static_assert(is_same<decltype(deg{}.root<1>()), rad>::value,
              "The 1st root should always return the base type");

static_assert(is_same<decltype(metre{}.pow<2>().root<2>()), metre>::value,
              "The 2nd root should work with even exponents");

static_assert(is_same<decltype((deg{} * rad{}).root<2>()), rad>::value,
              "The 2nd root should work with even exponents");

static_assert(!test_has_root<unit_pow_t<metre, 3>, 2>::value,
              "The 2nd root should not work with uneven exponents");

static_assert(is_same<decltype(metre{}.pow<4>().root<2>()), unit_pow_t<metre, 2>>::value,
              "The 2nd root should work with even exponents");

static_assert(!test_has_root<unit_mul_t<metre, unit_mul_t<deg, rad>>, 2>::value,
              "The 2nd root should not work with mixed even and uneven exponents");

static_assert(!test_has_root<metre, 3>::value,
              "The 3rd root should not work with exponents not dividable by 3");

static_assert(!test_has_root<unit_pow_t<metre, 2>, 3>::value,
              "The 3rd root should not work with exponents not dividable by 3");

static_assert(is_same<decltype(metre{}.pow<3>().root<3>()), metre>::value,
              "The 3rd root should work with exponents dividable by 3");

static_assert(!test_has_root<unit_pow_t<metre, 4>, 3>::value,
              "The 3rd root should not work with exponents not dividable by 3");

/* Test pow<N, D>() */

template <class Unit, sint ExpNum, sint ExpDenom, class = void>
struct test_has_pow : false_type {};

template <class Unit, sint ExpNum, sint ExpDenom>
struct test_has_pow<Unit, ExpNum, ExpDenom,
                    void_t<decltype(Unit{}.template pow<ExpNum, ExpDenom>())>>
    : true_type {};

static_assert(!test_has_pow<scalar, 0, 0>::value,
              "The 0 denominator in the exponent is forbidden");

static_assert(!test_has_pow<scalar, 1, 0>::value,
              "The 0 denominator in the exponent is forbidden");

static_assert(!test_has_pow<metre, 0, 0>::value,
              "The 0 denominator in the exponent is forbidden");

static_assert(!test_has_pow<metre, -1, 0>::value,
              "The 0 denominator in the exponent is forbidden");

static_assert(test_has_pow<metre, 0, 1>::value,
              "Any numerator should work with the 1 denominator");

static_assert(is_same<decltype(metre{}.pow<0, 1>()), scalar>::value,
              "The 0 numerator should have a scalar result");

static_assert(is_same<decltype(deg{}.pow<0, 1>()), scalar>::value,
              "The 0 numerator should have a scalar result");

static_assert(is_same<decltype(deg{}.pow<1, 1>()), rad>::value,
              "The 1 denominator should be equivalent to pow<N>");

static_assert(is_same<decltype(deg{}.pow<2, 1>()), unit_pow_t<rad, 2>>::value,
              "The 1 denominator should be equivalent to pow<N>");

static_assert(is_same<decltype(deg{}.pow<-1, 1>()), unit_pow_t<rad, -1>>::value,
              "The 1 denominator should be equivalent to pow<N>");

static_assert(is_same<decltype(deg{}.pow<-2, 1>()), unit_pow_t<rad, -2>>::value,
              "The 1 denominator should be equivalent to pow<N>");

static_assert(test_has_root<unit_pow_t<metre, 2>, 2>::value,
              "The 2nd root should work with even exponents");

static_assert(is_same<decltype(metre{}.pow<2, 2>()), metre>::value,
              "Identical numerator and denominator should behave like pow<1>");

static_assert(is_same<decltype(metre{}.pow<3, 3>()), metre>::value,
              "Identical numerator and denominator should behave like pow<1>");

static_assert(is_same<decltype(metre{}.pow<-1, -1>()), metre>::value,
              "Identical numerator and denominator should behave like pow<1>");

static_assert(is_same<decltype(metre{}.pow<-2, -2>()), metre>::value,
              "Identical numerator and denominator should behave like pow<1>");

static_assert(is_same<decltype((deg{}.pow<2>() * rad{}).pow<1, 3>()), rad>::value,
              "Exponents times numerator have to be dividable by the denominator");

static_assert(is_same<decltype((deg{}.pow<2>() * rad{}).pow<2, 3>()), unit_pow_t<rad, 2>>::value,
              "Exponents times numerator have to be dividable by the denominator");

static_assert(is_same<decltype((deg{}.pow<2>() * rad{}).pow<-1, 3>()), unit_pow_t<rad, -1>>::value,
              "Exponents times numerator have to be dividable by the denominator");

static_assert(!test_has_root<unit_pow_t<metre, 3>, 2>::value,
              "The 2nd root should not work with uneven exponents");

static_assert(is_same<decltype(deg{}.pow<4, 2>()), unit_pow_t<rad, 2>>::value,
              "Exponents times numerator have to be dividable by the denominator");

static_assert(is_same<decltype(deg{}.pow<2>().pow<3, 2>()), unit_pow_t<rad, 3>>::value,
              "Exponents times numerator have to be dividable by the denominator");

static_assert(!test_has_root<unit_mul_t<metre, unit_mul_t<deg, rad>>, 2>::value,
              "The 2nd root should not work with mixed even and uneven exponents");

static_assert(!test_has_root<metre, 3>::value,
              "The 3rd root should not work with exponents not dividable by 3");

static_assert(!test_has_root<unit_pow_t<metre, 2>, 3>::value,
              "The 3rd root should not work with exponents not dividable by 3");

static_assert(test_has_root<unit_pow_t<metre, 3>, 3>::value,
              "The 3rd root should work with exponents dividable by 3");

static_assert(is_same<decltype(deg{}.pow<3>().pow<1, 3>()), rad>::value,
              "Exponents times numerator have to be dividable by the denominator");

static_assert(is_same<decltype(deg{}.pow<3>().pow<2, -3>()), unit_pow_t<rad, -2>>::value,
              "Exponents times numerator have to be dividable by the denominator");

static_assert(!test_has_root<unit_pow_t<metre, 4>, 3>::value,
              "The 3rd root should not work with exponents not dividable by 3");

/* Test sqrt<>() */

template <class Unit, class = void>
struct test_has_sqrt : false_type {};

template <class Unit>
struct test_has_sqrt<Unit, void_t<decltype(sqrt(Unit{}))>> : true_type {};

static_assert(test_has_sqrt<unit_pow_t<deg, 0>>::value,
              "Units with even exponents should have an sqrt() member");

static_assert(!test_has_sqrt<unit_pow_t<deg, 1>>::value,
              "Units with uneven exponents should not have an sqrt() member");

static_assert(test_has_sqrt<unit_pow_t<deg, 2>>::value,
              "Units with even exponents should have an sqrt() member");

static_assert(!test_has_sqrt<unit_pow_t<deg, 3>>::value,
              "Units with uneven exponents should not have an sqrt() member");

static_assert(!test_has_sqrt<unit_pow_t<deg, -1>>::value,
              "Units with uneven exponents should not have an sqrt() member");

static_assert(is_same<decltype(sqrt(scalar{})), scalar>::value,
              "Test sqrt() return type");

static_assert(is_same<decltype(sqrt(kilo{})), scalar>::value,
              "Test sqrt() return type");

static_assert(is_same<decltype(sqrt(unit_mul_t<deg, rad>{})), rad>::value,
              "Test sqrt() return type");

static_assert(is_same<decltype(sqrt(unit_pow_t<deg, -2>{})), unit_pow_t<rad, -1>>::value,
              "Test sqrt() return type");

static_assert(is_same<decltype(sqrt(unit_pow_t<deg, 4>{})), unit_pow_t<rad, 2>>::value,
              "Test sqrt() return type");

/* Test cbrt<>() */

template <class Unit, class = void>
struct test_has_cbrt : false_type {};

template <class Unit>
struct test_has_cbrt<Unit, void_t<decltype(cbrt(Unit{}))>> : true_type {};

static_assert(test_has_cbrt<unit_pow_t<deg, 0>>::value,
              "Units with exponents dividable by 3 should have an cbrt() member");

static_assert(!test_has_cbrt<unit_pow_t<deg, 1>>::value,
              "Units with exponents not dividable by 3 should not have an cbrt() member");

static_assert(!test_has_cbrt<unit_pow_t<deg, 2>>::value,
              "Units with exponents not dividable by 3 should not have an cbrt() member");

static_assert(test_has_cbrt<unit_pow_t<deg, 3>>::value,
              "Units with exponents dividable by 3 should have an cbrt() member");

static_assert(!test_has_cbrt<unit_pow_t<deg, -1>>::value,
              "Units with exponents not dividable by 3 should not have an cbrt() member");

static_assert(is_same<decltype(cbrt(scalar{})), scalar>::value,
              "Test cbrt() return type");

static_assert(is_same<decltype(cbrt(kilo{})), scalar>::value,
              "Test cbrt() return type");

static_assert(is_same<decltype(cbrt(unit_mul_t<deg, unit_pow_t<rad, 2>>{})), rad>::value,
              "Test cbrt() return type");

static_assert(is_same<decltype(cbrt(unit_pow_t<deg, -3>{})), unit_pow_t<rad, -1>>::value,
              "Test cbrt() return type");

static_assert(is_same<decltype(cbrt(unit_pow_t<deg, 6>{})), unit_pow_t<rad, 2>>::value,
              "Test cbrt() return type");

/* Test pow(U, T), should only work for scalar units */

template <class Unit, class = void>
struct test_has_scalar_pow : false_type {};

template <class Unit>
struct test_has_scalar_pow<Unit, void_t<decltype(pow(Unit{}, 0))>> : true_type {};

static_assert(test_has_scalar_pow<scalar>::value,
              "Scalar values support arbitrary powers");

static_assert(!test_has_scalar_pow<metre>::value,
              "Non-scalar values do not support arbitrary powers");

static_assert(!test_has_scalar_pow<kilometre>::value,
              "Non-scalar values do not support arbitrary powers");

static_assert(!test_has_scalar_pow<deg>::value,
              "Non-scalar values do not support arbitrary powers");

/* Test hypot(U, U), should work with convertible units */

template <class Lhs, class Rhs, class = void>
struct test_has_hypot : false_type {};

template <class Lhs, class Rhs>
struct test_has_hypot<Lhs, Rhs, void_t<decltype(hypot(Lhs{}, Rhs{}))>>
    : true_type {};

static_assert(test_has_hypot<double, double>::value,
              "The hypot() function for double is defined by <cmath>");

static_assert(test_has_hypot<kilometre, metre>::value,
              "The hypot() function for convertible units should be defined");

static_assert(!test_has_hypot<kilometre, deg>::value,
              "The hypot() function for inconvertible units should not be defined");

static_assert(!test_has_hypot<kilometre, double>::value,
              "The hypot() function for inconvertible units should not be defined");

static_assert(!test_has_hypot<double, kilometre>::value,
              "The hypot() function for inconvertible units should not be defined");

static_assert(test_has_hypot<kilo, double>::value,
              "The hypot() function for convertible units should be defined");

template <class Return, class Lhs, class Rhs, class = Return>
struct test_ret_hypot : false_type {};

template <class Return, class Lhs, class Rhs>
struct test_ret_hypot<Return, Lhs, Rhs, decltype(hypot(Lhs{}, Rhs{}))>
    : true_type {};

static_assert(test_ret_hypot<double, double, double>::value,
              "Defined by <cmath>");

static_assert(test_ret_hypot<kilometre, kilometre, metre>::value,
              "Return type should be Lhs");

static_assert(test_ret_hypot<metre, metre, kilometre>::value,
              "Return type should be Lhs");

static_assert(test_ret_hypot<kilo, kilo, int>::value,
              "Return type should be Lhs");

static_assert(test_ret_hypot<kilo, int, kilo>::value,
              "Return type should be kilo");

/* Validation, these should pass. */

static_assert(validate_units<scalar, metre, kilogram, second,
                             rad, kilometre, hour, deg>::value,
              "Test units for making a valid set");

int main() {
	/*
	 * Runtime tests.
	 */

	/* Test % operator */
	assert((metre{5.} % metre{2.}) == metre{1.});
	assert((scalar{5.} % scalar{2.}) == scalar{1.});
	assert((scalar{5.} % 2.) == scalar{1.});
	assert((scalar{5.} % 2) == scalar{1.});
	assert((metre{-5.5} % metre{2.}) == metre{-1.5});
	assert((scalar{-5.5} % scalar{2.}) == scalar{-1.5});
	assert((scalar{-5.5} % 2.) == scalar{-1.5});
	assert((scalar{-5.5} % 2) == scalar{-1.5});
	assert((metre{1337.} % kilometre{1.}) == metre{337.});
	assert((-5.5 % scalar{2.}) == scalar{-1.5});
	assert((-5 % scalar{2.}) == scalar{-1});

	/* Test = assignment */
	assert((metre{5.} = metre{3.}) == metre{3.});
	assert((metre{5.} = kilometre{.5}) == metre{500.});
	assert((kilometre{5.} = metre{500.}) == kilometre{.5});
	assert((scalar{2.} = scalar{3.}) == scalar{3.});
	assert((scalar{2.} = 3.) == scalar{3.});
	assert((scalar{2.} = 3) == scalar{3.});

	/* Test += assignment */
	assert((metre{2.} += metre{3.}) == metre{5.});
	assert((metre{2.} += kilometre{.5}) == metre{502.});
	assert((kilometre{2.} += metre{500.}) == kilometre{2.5});
	assert((scalar{2.} += scalar{3.}) == scalar{5.});
	assert((scalar{2.} += 3.) == scalar{5.});
	assert((scalar{2.} += 3) == scalar{5.});

	/* Test -= assignment */
	assert((metre{5.} -= metre{2.}) == metre{3.});
	assert((metre{2.} -= kilometre{.5}) == metre{- 498.});
	assert((kilometre{2.} -= metre{500.}) == kilometre{1.5});
	assert((scalar{5.} -= scalar{2.}) == scalar{3.});
	assert((scalar{5.} -= 2.) == scalar{3.});
	assert((scalar{5.} -= 2) == scalar{3.});

	/* Test *= assignment */
	assert((metre{3.} *= scalar{2.}) == metre{6.});
	assert((metre{3.} *= 2.) == metre{6.});
	assert((metre{3.} *= 2) == metre{6.});
	assert((scalar{3.} *= scalar{2.}) == scalar{6.});
	assert((scalar{3.} *= 2.) == scalar{6.});
	assert((scalar{3.} *= 2) == scalar{6.});

	/* Test /= assignment */
	assert((metre{6.} /= scalar{2.}) == metre{3.});
	assert((metre{6.} /= 2.) == metre{3.});
	assert((metre{6.} /= 2) == metre{3.});
	assert((scalar{6.} /= scalar{2.}) == scalar{3.});
	assert((scalar{6.} /= 2.) == scalar{3.});
	assert((scalar{6.} /= 2) == scalar{3.});

	/* Test %= assignment */
	assert((metre{5.} %= metre{2.}) == metre{1.});
	assert((scalar{5.} %= scalar{2.}) == scalar{1.});
	assert((scalar{5.} %= 2.) == scalar{1.});
	assert((scalar{5.} %= 2) == scalar{1.});
	assert((metre{-5.5} %= metre{2.}) == metre{-1.5});
	assert((scalar{-5.5} %= scalar{2.}) == scalar{-1.5});
	assert((scalar{-5.5} %= 2.) == scalar{-1.5});
	assert((scalar{-5.5} %= 2) == scalar{-1.5});
	assert((metre{1337.} %= kilometre{1.}) == metre{337.});

	/* Test root<N>() */
	assert(scalar{8.}.root<3>() == 2.);
	assert((unit_pow_t<metre, 3>{8.}.root<3>() == metre{2.}));
	assert(abs(unit_pow_t<kilometre, 3>{8.}.root<3>() -
	           kilometre{2.}) < kilometre{.000000001});
	assert(scalar{8.}.root<-3>() == .5);
	assert((unit_pow_t<metre, -3>{8.}.root<-3>() == metre{.5}));
	assert(abs(unit_pow_t<kilometre, -3>{8.}.root<-3>() -
	           kilometre{.5}).value < .000001);

	/* Test pow<N, D>() for fractions */
	assert((scalar{8.}.pow<1, 3>() == 2.));
	assert((unit_pow_t<metre, 3>{8.}.pow<1, 3>() == metre{2.}));
	assert(abs(unit_pow_t<kilometre, 3>{8.}.pow<1, 3>() -
	           kilometre{2.}).value < .000001);
	assert(abs(scalar{8.}.pow<2, 3>() - 4.) < 0.000001);
	assert(abs(unit_pow_t<metre, 3>{8.}.pow<2, 3>() -
	           unit_pow_t<metre, 2>{4.}).value < 0.000001);
	assert(abs(unit_pow_t<kilometre, 3>{8.}.pow<2, 3>() -
	           unit_pow_t<kilometre, 2>{4.}).value < .000001);

	/* Test <cmath> functions */
	assert(exp(scalar{0.}) == 1.);
	assert(exp(kilo{0.}) == 1.);
	assert(exp(scalar{1.}) == exp(1.));
	assert(exp(kilo{1.}) == exp(1000.));
	int exp = 0;
	assert(frexp(scalar{6.}, &exp) == .75 && exp == 3);
	assert(frexp(kilometre{6.}, &exp) == kilometre{.75} && exp == 3);
	assert(ldexp(scalar{3.}, 3) == 24.);
	assert(ldexp(kilometre{3.}, 3) == kilometre{24.});
	assert(log(scalar{5.}) == log(5.));
	assert(log(kilo{5.}) == log(5000.));
	assert(log10(scalar{100.}) == 2.);
	assert(log10(kilo{100.}) == 5.);
	auto s_intpart = scalar{0.};
	assert(modf(scalar{12.125}, s_intpart) == .125 && s_intpart == 12.);
	auto km_intpart = kilometre{0.};
	assert(modf(kilometre{12.125}, km_intpart) == kilometre{.125} &&
	       km_intpart == kilometre{12.});
	assert(exp2(scalar{16.}) == 65536.);
	assert(exp2(kilo{1.}) == exp2(1000.));
	assert(log1p(scalar{16.}) == log1p(16.));
	assert(log1p(kilo{1.}) == log1p(1000.));
	assert(log2(scalar{1024.}) == 10.);
	assert(log2(kilo{1.024}) == 10.);
	assert(logb(scalar{1024.}) == logb(1024.));
	assert(logb(kilo{1.}) == logb(1000.));
	assert(scalbn(scalar{1.125}, 10) == scalbn(1.125, 10));
	assert(scalbn(kilometre{1.024}, -10) == metre{scalbn(1024, -10)});
	assert(scalbln(scalar{1.125}, 10) == scalbn(1.125, 10));
	assert(scalbln(kilometre{1.024}, -10) == metre{scalbn(1024, -10)});
	assert(pow(scalar{4.}, 1.5) == 8.);
	assert(pow(kilo{10.}, .5) == 100.);
	assert(sqrt(scalar{16.}) == 4.);
	assert(sqrt(kilo{10.}) == 100.);
	assert(sqrt(kilometre{1.6} * metre{900.}) == kilometre{1.2});
	assert(cbrt(scalar{64.}) == 4.);
	assert(cbrt(kilo{1.}) == 10.);
	assert(cbrt(rad{2.} * rad{3.} * rad{4.5}) == rad{3.});
	assert(hypot(kilometre{3.}, metre{4000.}) == kilometre{5.});
	assert(hypot(kilo{3.}, 4000.) == kilo{5.});
	assert(hypot(3000., kilo{4.}) == kilo{5.});
	assert(ceil(deg{2.789}) == deg{3.});
	assert(ceil(kilometre{2.789}) == metre{3000.});
	assert(floor(deg{2.789}) == deg{2.});
	assert(floor(kilometre{2.789}) == metre{2000.});
	assert(fmod(metre{1337.}, kilometre{1.}) == metre{337.});
	assert(fmod(kilo{1.5}, 1000.) == 500.);
	assert(trunc(kilometre{12.5}) == metre{12000.});
	assert(trunc(kilometre{-12.5}) == metre{-12000.});
	assert(round(kilometre{12.5}) == metre{13000.});
	assert(round(kilometre{12.4999}) == metre{12000.});
	/* BROKEN by design: https://llvm.org/bugs/show_bug.cgi?id=8100
	for (auto const mode : {FE_DOWNWARD, FE_TONEAREST, FE_TOWARDZERO, FE_UPWARD}) {
		fesetround(mode);
		switch (mode) {
		case FE_DOWNWARD:
			assert(rint(kilometre{12.9}) == metre{12000.} && "FE_DOWNWARD");
			assert(rint(kilometre{-12.1}) == metre{-13000.} && "FE_DOWNWARD");
			assert(nearbyint(kilometre{12.9}) == metre{12000.} && "FE_DOWNWARD");
			assert(nearbyint(kilometre{-12.1}) == metre{-13000.} && "FE_DOWNWARD");
			break;
		case FE_TONEAREST:
			assert(rint(kilometre{12.4}) == metre{12000.} && "FE_TONEAREST");
			assert(rint(kilometre{-12.4}) == metre{-12000.} && "FE_TONEAREST");
			assert(nearbyint(kilometre{12.4}) == metre{12000.} && "FE_TONEAREST");
			assert(nearbyint(kilometre{-12.4}) == metre{-12000.} && "FE_TONEAREST");
			break;
		case FE_TOWARDZERO:
			assert(rint(kilometre{12.9}) == metre{12000.} && "FE_TOWARDZERO");
			assert(rint(kilometre{-12.9}) == metre{-12000.} && "FE_TOWARDZERO");
			assert(nearbyint(kilometre{12.9}) == metre{12000.} && "FE_TOWARDZERO");
			assert(nearbyint(kilometre{-12.9}) == metre{-12000.} && "FE_TOWARDZERO");
			break;
		case FE_UPWARD:
			assert(rint(kilometre{12.1}) == metre{13000.} && "FE_UPWARD");
			assert(rint(kilometre{-12.9}) == metre{-12000.} && "FE_UPWARD");
			assert(nearbyint(kilometre{12.1}) == metre{13000.} && "FE_UPWARD");
			assert(nearbyint(kilometre{-12.9}) == metre{-12000.} && "FE_UPWARD");
			break;
		}
	}
	*/
	assert(remainder(metre{1337.}, kilometre{1.}) == metre{337.});
	assert(remainder(kilo{1.5}, 1000.) == -500.);
	int quot = 0;
	assert(remquo(metre{1337.}, kilometre{1.}, &quot) == metre{337.} &&
	       quot == 1);
	assert(remquo(kilo{1.5}, 1000., &quot) == -500. &&
	       quot == 2);
	assert(kilometre{42.} - nextafter(kilometre{42.}, metre{0.}) > metre{0.});
	assert(kilometre{42.} - nextafter(kilometre{42.}, metre{0.}) < metre{.000000001});
	assert(nextafter(kilometre{42.}, kilometre{43.}) - kilometre{42.} > metre{0.});
	assert(nextafter(kilometre{42.}, kilometre{43.}) - kilometre{42.} < metre{.000000001});
	assert(kilo{42.} - nextafter(kilo{42.}, 43.) > 0.);
	assert(kilo{42.} - nextafter(kilo{42.}, 43.) < .000000001);
	assert(fdim(kilometre{42.}, metre{41875.}) == metre{125.});
	assert(fdim(kilometre{42.}, metre{42125.}) == metre{0.});
	assert(fdim(kilo{42.}, 41875.) == 125.);
	assert(fdim(kilo{42.}, 42125.) == 0.);
	assert(fmax(kilometre{42.}, metre{43.}) == kilometre{42.});
	assert(fmax(kilo{42.}, 43.) == kilo{42.});
	assert(fmin(kilometre{42.}, metre{43.}) == metre{43.});
	assert(fmin(kilo{42.}, 43.) == 43.);
	assert(fabs(kilometre{42.}) == kilometre{42.});
	assert(fabs(kilometre{-42.}) == kilometre{42.});
	assert(fabs(kilo{-42.}) == 42000.);
	assert(abs(kilometre{42.}) == kilometre{42.});
	assert(abs(kilometre{-42.}) == kilometre{42.});
	assert(abs(kilo{-42.}) == 42000.);
	assert(fma(metre{10.}, second{2.}, kilometre{1.} * hour{1.}) ==
	       metre{3600020.} * second{1.});
	assert(fma(kilometre{12.}, 3., kilometre{6.}) == kilometre{42.});
	assert(fma(3., kilometre{12.}, kilometre{6.}) == kilometre{42.});
	assert(fma(kilo{12.}, 3., 6.) == 36006.);
	assert(fma(3., kilo{12.}, 6.) == 36006.);

	return 0;
}

