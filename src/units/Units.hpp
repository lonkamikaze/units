/** \file
 * Implements the units::unit template
 */

#ifndef _UNITS_UNITS_HPP_
#define _UNITS_UNITS_HPP_

#include <cmath>       /* fmod(), pow() */
#include <type_traits> /* std::enable_if, std::true_type, std::false_type */

/* clang-format off */
/**
 * This namespace provides templates to create custom \ref unit types.
 *
 * The central template is the \ref unit template, it provides type safe
 * compile time dimension safety, unit derivation and conversion.
 *
 * E.g. it provides the ability to divide metres by seconds and implicitly
 * convert them to km per hour, when needed. These unit conversions happen
 * only when required, i.e. m/s is a different type than km/h. However, km/h
 * can be converted to m/s and even miles per hour and vice versa, whereas
 * m/s² would (correctly) fail to convert to m/s at compile time.
 *
 * Defining Base Units
 * -------------------
 *
 * It builds upon the \ref exponents_chain, \ref rational and
 * \ref constants_chain templates.
 *
 * The \ref exponents_chain template provides the possibility to store signed
 * integers in a type:
 *
 * ~~~~{.cpp}
 * exponents_chain<23, exponents_chain<42, exponents_chain<1337, void>>>
 * ~~~~
 *
 * For convenience the \ref exponents type factory can generate the same chain
 * using variadic template arguments:
 *
 * ~~~~{.cpp}
 * exponents_t<23, 42, 1337>
 * ~~~~
 *
 * The \ref exponents_chain is used in conjunction with the \ref unit template
 * to provide it as a list of exponents for each base unit. This is used in
 * unit conversion to ensure that only units with the same base units are
 * convertible and in multiplication and division to generate derived units.
 *
 * The following example illustrates the definition of three units that
 * represent base units:
 *
 * ~~~~{.cpp}
 * typedef unit<double, exponents_t<1, 0, 0>, rational<1, 1>> metre;
 * 
 * typedef unit<double, exponents_t<0, 1, 0>, rational<1, 1>> kilogram;
 * 
 * typedef unit<double, exponents_t<0, 0, 1>, rational<1, 1>> second;
 * ~~~~
 *
 * Defining Units with Conversion Factors
 * --------------------------------------
 *
 * The unit type expects the following arguments:
 *
 * - An encapsulated type, usually a floating-point type, but it could be any
 *   numerical type
 * - An \ref exponents_chain with the exponents of each dimension, e.g. space,
 *   time, rotation, mass etc.
 * - A \ref rational factor representing 1 in the current unit in base units
 * - Optionally a \ref constants_pack (see below)
 *
 * The conversion factor can be used to create additional units with the same
 * dimensions:
 *
 * ~~~~{.cpp}
 * typedef unit<double, exponents_t<1, 0, 0>, rational<1000, 1>> kilometre;
 * 
 * typedef unit<double, exponents_t<0, 1, 0>, rational<1, 1000>> gram;
 * 
 * typedef unit<double, exponents_t<0, 0, 1>, rational<60, 1>> minute;
 * ~~~~
 *
 * The conversion factor is used by \ref unit to provide implicit conversions
 * between units:
 *
 * ~~~~{.cpp}
 * second{12.} + minute{1.} == second{72.}
 * ~~~~
 *
 * By multiplying or dividing two unit instances, types with their
 * combined dimensions and factors are created:
 *
 * ~~~~{.cpp}
 * kilometre{1.5} / (minute{1} * minute{1}) ==
 * unit<double, exponents_t<1, 0, -2>, rational<1000, 60 * 60>>{1.5}
 * ~~~~
 *
 * Creating User-Defined Literals
 * ------------------------------
 *
 * The unit types can be used in combination with user-defined literals:
 *
 * ~~~~{.cpp}
 * metre operator "" _m(long double const op) {return metre{double(op)}};
 * second operator "" _s(long double const op) {return second{double(op)}};
 * typedef decltype(1._m/(1._s*1._s)) mps2;
 * mps2 const g = 9.81_m/(1._s*1._s);
 * ~~~~
 *
 * Accessing the Raw Value
 * -----------------------
 *
 * To retrieve the scalar value from a unit instance convert to the desired
 * unit and retrieve it through the value member:
 *
 * ~~~~{.cpp}
 * double time = second{5._min}.value; // 300 seconds
 * ~~~~
 *
 * Using Constants as Conversion Factors
 * -------------------------------------
 *
 * If rational conversion factors do not suffice, custom constants can
 * be used:
 *
 * ~~~~{.cpp}
 * struct const_Pi {
 * 	static constexpr double const value{3.14159265358979323846};
 * };
 * ~~~~
 *
 * The constants need to be assembled into a list, with the
 * \ref constants_chain template. Just like with the \ref exponents_chain
 * template there is a \ref constants type factory. To use that chain in a
 * unit it needs to be packed with a set of exponents. A custom template like
 * in the following example can make that more convenient.
 *
 * ~~~~{.cpp}
 * template <sint ... Exponents>
 * using unit_constants_t =
 *       constants_pack_t<constants_t<const_Pi>, Exponents ...>;
 * ~~~~
 *
 * The following example extends the previous set of dimensions with
 * dimensions of angles. The base definition is the radiant angle and
 * degree is defined as
 * \f$ rad = deg \times \frac{1}{180} \times \pi^{1}\f$:
 *
 * ~~~~{.cpp}
 * typedef unit<double, exponents_t<0, 0, 0, 1>, rational<1, 1>,
 *              unit_constants_t<0>> rad;
 * 
 * typedef unit<double, exponents_t<0, 0, 0, 1>, rational<1, 180>,
 *              unit_constants_t<1>> deg;
 * ~~~~
 *
 * In order for different units to be compatible, they must have the same
 * number of exponents and use the same set of constants. So the previous
 * units' definitions need to be changed to achieve compatibility with the
 * \c rad and \c deg units.
 *
 * ~~~~{.cpp}
 * typedef unit<double, exponents_t<0, 0, 0>, rational<1, 1>,
 *              unit_constants_t<0>> scalar;
 * typedef unit<double, exponents_t<0, 0, 0>, rational<1000, 1>,
 *              unit_constants_t<0>> kilo;
 *
 * typedef unit<double, exponents_t<1, 0, 0>, rational<1000, 1>,
 * typedef unit<double, exponents_t<1, 0, 0>, rational<1, 1>,
 *              unit_constants_t<0>> metre;
 * typedef unit<double, exponents_t<1, 0, 0>, rational<1000, 1>,
 *              unit_constants_t<0>> kilometre;
 * 
 * typedef unit<double, exponents_t<0, 1, 0>, rational<1, 1>,
 *              unit_constants_t<0>> kilogram;
 * typedef unit<double, exponents_t<0, 1, 0>, rational<1, 1000>,
 *              unit_constants_t<0>> gram;
 * 
 * typedef unit<double, exponents_t<0, 0, 1>, rational<1, 1>,
 *              unit_constants_t<0>> second;
 * typedef unit<double, exponents_t<0, 0, 1>, rational<60, 1>,
 *              unit_constants_t<0>> minute;
 * ~~~~
 *
 * Validating a Set of Units
 * -------------------------
 *
 * To ensure that the defined units are compatible they can be run through
 * the \ref validate_units test:
 *
 * ~~~~{.cpp}
 * static_assert(validate_units<metre, kilometre, kilogram, gram,
 *                              second, minute, rad, deg>::value,
 *               "Test units for making a valid set");
 * ~~~~
 *
 * Finding the offending type in the diagnostics can be problematic, that is
 * why the test keeps a running count of the tested types, which can be found
 * in the diagnostics output:
 *
 *     error: static_assert failed "The current given type is not a valid unit template instance"
 *             static_assert(is_unit<Lhs>::value,
 *             ^             ~~~~~~~~~~~~~~~~~~~
 *     note: in instantiation of template class 'units::validate_units_count<4, ...
 *
 * The 4 argument of \ref validate_units_count means that the fourth given
 * type is not a valid unit.
 *
 * When two types are valid \ref unit types, but not compatible, a similar
 * message appears:
 *
 *     error: static_assert failed "The given types are not compatible units"
 *             static_assert(is_compatible_unit<Lhs, Rhs>::value,
 *             ^             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *     note: in instantiation of template class 'units::validate_units_count<6, ...
 *
 * The 6 means the sixth given type is incompatible with the seventh given
 * type.
 *
 * If there are multiple problems with the provided types, problems with the
 * last types are found first.
 *
 * Operators, Members and Friends
 * ------------------------------
 *
 * Unit typed values support a number of operators. The unary + and - operators
 * and the following list of binary operators:
 *
 * | Operator | Constexpr | Same Unit | Any Unit | Scalar   | Returns      |
 * |----------|-----------|-----------|----------|----------|--------------|
 * | +, -     | yes       | yes       |          |          | same unit    |
 * | *, /     | yes       |           | yes      | yes      | derived unit |
 * | %        | dependant | yes       |          |          | same unit    |
 * | +=, -=   |           | yes       |          |          | same unit &  |
 * | *=, /=   |           |           |          | yes      | same unit &  |
 * | %=       |           | yes       |          |          | same unit &  |
 * | ==, !=   | yes       | yes       |          |          | bool         |
 * | <, <=    | yes       | yes       |          |          | bool         |
 * | >, >=    | yes       | yes       |          |          | bool         |
 *
 * Note that scalar units and the underlying primitive type work
 * interchangeably. Wherever the left hand unit is a scalar unit or a related
 * unit (one that can be cast to the scalar unit), primitive operands are
 * implicitly cast to the scalar unit, even for the operators that do not
 * support scalar operands explicitly.
 *
 * The same unit operand can be satisfied by any unit related by way of
 * implicit construction.
 *
 * A notable member function is unit::pow<int>(), which takes the unit to an
 * integer power. This function guarantees to be constexpr provided the
 * underlying type supports constexpr multiplication.
 *
 * The related members unit::root<int>() and unit::pow<int,int>() both
 * use `T pow(T, T)` (`T` is the underlying type). So they only
 * are constexpr if `T pow(T, T)` is.
 *
 * The unit::root<int>() member is only available for roots that divide
 * the base unit exponents without a remainder.
 *
 * The unit::pow<int,int>() template arguments create a fraction. This
 * fraction is multiplied with the base unit exponents. The member function
 * is only available for units where the resulting base units are integral
 * values.
 *
 * ~~~~{.cpp}
 * // works, is constexpr
 * constexpr auto const g = 9.81_m/(1._s).pow<2>();
 * // fails, not a constant expression
 * constexpr auto const c = ((3._m).pow<2>() + (4._m).pow<2>()).root<2>();
 * // works
 * auto const c = ((3._m).pow<2>() + (4._m).pow<2>()).root<2>();
 * // fails, sqrt(m) is not a valid unit
 * auto const x1 = (9._m).root<2>();
 * // works, m^2 is a valid unit
 * auto const x2 = (4._m * 5._m * 6._m).pow<2, 3>();
 * // fails, m^(3/2) is not a valid unit
 * auto const x3 = (4._m * 5._m * 6._m).pow<1, 2>();
 * ~~~~
 *
 * The \ref unit template also provides overloads for many of the functions
 * provided by \<cmath\>. These functions fall back to the functions of the
 * same name for the underlying type.
 *
 * @see units_unit_cmath
 *
 * This covers all the functions that make sense. Some of them only work for
 * units related to the scalar unit (e.g. `exp()`). Others have limitations
 * similar to the unit::root<int>() function. E.g. the `sqrt()` and `cbrt()`
 * functions.
 *
 * Some units fit into the unit concept seamlessly:
 *
 * ~~~~{.cpp}
 * auto const c = hypot(3._m, 4._m); // == 5._m
 * ~~~~
 *
 * Other functions, e.g. those that cast the result to an integer like
 * `lround()` have been omitted.
 *
 * Also omitted have been angular functions like `sin()` or `cos()`. These
 * overloads should be provided together with a particular unit system to
 * work with a `radian` angular unit.
 *
 */ /* clang-format on */
namespace units {

using std::enable_if;
using std::true_type;
using std::false_type;
using std::is_same;

/*
 * Generic SFINAE
 */

/**
 * Alias for std::enable_if, provided by C++14, but not C++11.
 */
template <bool Cond, typename T = void>
using enable_if_t = typename enable_if<Cond, T>::type;

/**
 * Common SFINAE idiom, provided by C++17, but not C++11.
 */
template <class...>
using void_t = void;

/**
 * Compares the length of two chained types.
 *
 * The chains have to be void terminated.
 *
 * @tparam Lhs,Rhs
 *	The chained type
 */
template <class Lhs, class Rhs, class = void>
struct is_same_length : false_type {};

/**
 * Recursively follow both chained types.
 *
 * @see is_same_length
 */
template <class Lhs, class Rhs>
struct is_same_length<Lhs, Rhs, void_t<typename Lhs::tail, typename Rhs::tail>>
    : is_same_length<typename Lhs::tail, typename Rhs::tail> {};

/**
 * Terminates the recursion.
 *
 * @see is_same_length
 */
template <>
struct is_same_length<void, void> : true_type {};

/**
 * Tests whether a given type supports the modulo operator.
 *
 * @tparam T
 *	The type to test
 */
template <typename T, class = void>
struct has_op_modulo : false_type {};

/**
 * Return true if ``T{} % T{}`` is valid code.
 *
 * @see has_op_modulo
 */
template <typename T>
struct has_op_modulo<T, void_t<decltype(T{} % T{})>> : true_type {};

/**
 * Tests whether a given type supports the modulo assignment operator.
 *
 * @tparam T
 *	The type to test
 */
template <typename T, class = void>
struct has_op_assign_modulo : false_type {};

/**
 * Return true if ``T{} %= T{}`` is valid code.
 *
 * @see has_op_assign_modulo
 */
template <typename T>
struct has_op_assign_modulo<T, void_t<decltype(T{} %= T{})>> : true_type {};

/*
 * Big integers for template arguments
 */

/**
 * Signed integer type for template arguments.
 */
typedef long long sint;

/**
 * Unsigned integer type for template arguments.
 */
typedef unsigned long long uint;

/*
 * Rational number structure
 */

/**
 * Implements Euclid's algorithm to find the GCD between two unsigned integers.
 *
 * @param a,b
 *	The values to get the GCD for
 * @return
 *	The GCD of a and b
 */
constexpr uint euclid(uint const a, uint const b) {
	return b ? euclid(b, a % b) : a;
}

/**
 * Compile time, self-minimising rational numbers.
 *
 * @tparam Numerator,Denominator
 *	The numerator and denominator of this rational number
 */
template <uint Numerator, uint Denominator>
struct rational {

	/**
	 * The minimised numerator trait.
	 */
	static constexpr uint const numerator{
	    Numerator / euclid(Numerator, Denominator)};

	/**
	 * The minimised denominator trait.
	 */
	static constexpr uint const denominator{
	    Denominator / euclid(Numerator, Denominator)};

	/**
	 * The minimised version of the type, for comparisons.
	 */
	typedef rational<numerator, denominator> type;

	/**
	 * Instances can be cast to numerical types.
	 *
	 * @tparam T
	 *	The type to cast to
	 * @return
	 *	The rational number converted to a real value
	 */
	template <typename T>
	constexpr explicit operator T() const {
		return T(this->numerator) / T(this->denominator);
	}
};

/**
 * Checks whether the given type is an instance of the rational template.
 *
 * @tparam Op
 *	The type to check
 */
template <class Op>
struct is_rational : false_type {};

/**
 * Matches instances of the rational template.
 *
 * @tparam Numerator,Denominator
 *	The given numerator and denominator of the rational type definiton
 * @see is_rational
 */
template <uint Numerator, uint Denominator>
struct is_rational<rational<Numerator, Denominator>> : true_type {};

/**
 * Compares two rationals for representing the same value.
 *
 * E.g. rational<1, 2> is a different type than rational<2, 4>, but they
 * represent the same value.
 *
 * @tparam Lhs,Rhs
 *	The rational types to compare
 */
template <class Lhs, class Rhs, class = void>
struct is_same_rational : false_type {};

/**
 * Performs the comparison.
 *
 * @see is_same_rational
 */
template <class Lhs, class Rhs>
struct is_same_rational<
    Lhs, Rhs, enable_if_t<is_rational<Lhs>::value && is_rational<Rhs>::value>>
    : is_same<typename Lhs::type, typename Rhs::type> {};

/**
 * Unary operations on a rational type.
 *
 * @tparam Op
 *	The rational type
 */
template <class Op, class = enable_if_t<is_rational<Op>::value>>
struct rational_unary {
	/**
	 * Returns the inverse rational type.
	 */
	typedef rational<Op::denominator, Op::numerator> invert;
};

/**
 * Template alias for rational_unary<>::invert.
 *
 * @tparam Args
 *	The arguments to pass on to rational_unary
 */
template <class... Args>
using rational_invert_t = typename rational_unary<Args...>::invert;

/**
 * Multiplication on rational types.
 *
 * @tparam Lhs,Rhs
 *	The two rational types to multiply
 */
template <class Lhs, class Rhs, class = enable_if_t<is_rational<Lhs>::value &&
                                                    is_rational<Rhs>::value>>
struct rational_mul {
	static_assert((Lhs::numerator * Rhs::numerator) / Rhs::numerator ==
	                  Lhs::numerator,
	              "Overflow in rational numerator multiplication");

	static_assert((Lhs::denominator * Rhs::denominator) /
	                      Rhs::denominator ==
	                  Lhs::denominator,
	              "Overflow in rational denominator multiplication");

	/**
	 * The type resulting from multiplication.
	 */
	typedef rational<Lhs::numerator * Rhs::numerator,
	                 Lhs::denominator * Rhs::denominator> type;
};

/**
 * Template alias for rational_mul<>::type.
 *
 * @tparam Args
 *	The arguments to pass to rational_mul
 */
template <class... Args>
using rational_mul_t = typename rational_mul<Args...>::type;

/**
 * Template alias for rational_mul<>::type, that realises rational division.
 *
 * @tparam Lhs, Rhs
 *	The arguments to pass to rational_mul
 */
template <class Lhs, class Rhs>
using rational_div_t = rational_mul_t<Lhs, rational_invert_t<Rhs>>;

/*
 * Exponents data structure
 */

/** @cond */
template <sint Exponent, class Tail = void, class = void>
struct exponents_chain;
/** @endcond */

/**
 * Recursive test for the exponents_chain template.
 *
 * @tparam Op
 *	The type to check for being a exponents_chain
 */
template <class Op>
struct is_exponents_chain : false_type {};

/**
 * Recursively check the chain.
 *
 * @tparam Exponent,Tail
 *	The exponent and the tail of this chain link
 * @see is_exponents_chain
 */
template <sint Exponent, class Tail>
struct is_exponents_chain<exponents_chain<Exponent, Tail>>
    : is_exponents_chain<Tail> {};

/**
 * Return true when reaching the end of chain.
 *
 * @see is_exponents_chain
 */
template <>
struct is_exponents_chain<void> : true_type {};

/**
 * This type provides a way to recursively store signed integers to use
 * them as a set of exponents to a set of bases.
 *
 * Use the \ref exponents_t factory to actually create one.
 *
 * @tparam Exponent
 *	The value of this link of the chain
 * @tparam Tail
 *	The remainder of the chain
 */
template <sint Exponent, class Tail>
struct exponents_chain /** @cond */<
    Exponent, Tail, enable_if_t<is_exponents_chain<Tail>::value>>
    /** @endcond */ {
	/**
	 * The remaining chain.
	 */
	typedef Tail tail;

	/**
	 * The power of this chain link.
	 */
	static constexpr sint const value{Exponent};
};

/**
 * Creates an exponents_chain from a set of exponents.
 *
 * @tparam Exponents
 *	A list of exponents
 */
template <sint... Exponents>
struct exponents {
	/**
	 * The exponents_chain type.
	 */
	typedef void type;
};

/**
 * Recursively create an exponents_chain.
 *
 * @tparam Exponent,Tail
 *	The head power and the tail of exponents
 * @see exponents
 */
template <sint Exponent, sint... Tail>
struct exponents<Exponent, Tail...> {
	/**
	 * The resulting exponents_chain
	 */
	typedef exponents_chain<Exponent, typename exponents<Tail...>::type>
	    type;
};

/**
 * Template alias for exponents<>::type, which returns an exponents_chain.
 *
 * @tparam Exponents
 *	The arguments to pass to exponents
 */
template <sint... Exponents>
using exponents_t = typename exponents<Exponents...>::type;

/**
 * Unary operators for exponents_chain types.
 *
 * @tparam Op
 *	The exponents_chain type
 */
template <class Op, class = enable_if_t<is_exponents_chain<Op>::value>>
struct exponents_unary {
	/**
	 * A exponents_chain type with all values 0.
	 */
	typedef exponents_chain<
	    0, typename exponents_unary<typename Op::tail>::zero> zero;

	/**
	 * A exponents_chain type with all values negated.
	 */
	typedef exponents_chain<
	    -Op::value, typename exponents_unary<typename Op::tail>::negate>
	    negate;
};

/**
 * Terminate recursion through the exponents_chain.
 *
 * @see exponents_unary
 */
template <>
struct exponents_unary<void> {
	/**
	 * Terminate recursion.
	 */
	typedef void zero;

	/**
	 * Terminate recursion.
	 */
	typedef void negate;
};

/**
 * Template alias for exponents_unary<>::zero.
 *
 * @tparam Op
 *	The exponents_chain to pass to exponents_unary
 */
template <class Op>
using exponents_zero_t = typename exponents_unary<Op>::zero;

/**
 * Template alias for exponents_unary<>::negate.
 *
 * @tparam Op
 *	The exponents_chain to pass to exponents_unary
 */
template <class Op>
using exponents_negate_t = typename exponents_unary<Op>::negate;

/**
 * Binary operators for exponents_chain types.
 *
 * @tparam Lhs,Rhs
 *	The exponents_chain types
 */
template <class Lhs, class Rhs,
          class = enable_if_t<is_exponents_chain<Lhs>::value &&
                              is_exponents_chain<Rhs>::value &&
                              is_same_length<Lhs, Rhs>::value>>
struct exponents_binary {
	/**
	 * Adds each value in Lhs to the corresponding value in Rhs.
	 */
	typedef exponents_chain<
	    Lhs::value + Rhs::value,
	    typename exponents_binary<typename Lhs::tail,
	                              typename Rhs::tail>::add> add;
};

/**
 * Terminate recursions of exponents_chain%s.
 *
 * @see exponents_binary
 */
template <>
struct exponents_binary<void, void> {
	/**
	 * Terminate recursion.
	 */
	typedef void add;
};

/**
 * Template alias for exponents_binary<>::add.
 *
 * @tparam Lhs,Rhs
 *	The exponents_chain types to pass to exponents_binary
 */
template <class Lhs, class Rhs>
using exponents_add_t = typename exponents_binary<Lhs, Rhs>::add;

/**
 * Template alias for exponents_binary<>::add, returning differences instead of
 * sums.
 *
 * @tparam Lhs,Rhs
 *	The exponents_chain types to pass to exponents_binary
 */
template <class Lhs, class Rhs>
using exponents_sub_t = exponents_add_t<Lhs, exponents_negate_t<Rhs>>;

/**
 * Returns the remainders of a list of exponents divided by a divisor.
 *
 * This is used by exponents_div::exact to test whether exponents are
 * dividable.
 *
 * @tparam Exponents
 *	The list of integer exponents
 * @tparam Divisor
 *	The divisor to divide by
 */
template <
    class Exponents, sint Divisor,
    class = enable_if_t<is_exponents_chain<Exponents>::value && Divisor != 0>>
struct exponents_mod {
	/**
	 * The remainders of the given exponents.
	 */
	typedef exponents_chain<
	    Exponents::value % Divisor,
	    typename exponents_mod<typename Exponents::tail, Divisor>::type>
	    type;
};

/**
 * Terminates the recursion.
 *
 * @tparam Divisor
 *	The divisor to divide by
 * @see exponents_mod
 */
template <sint Divisor>
struct exponents_mod<void, Divisor> {
	/**
	 * The result list is void terminated.
	 */
	typedef void type;
};

/**
 * Template alias for exponents_mod<>::type.
 *
 * @tparam Exponents,Divisor
 *	The arguments to exponents_mod
 */
template <class Exponents, sint Divisor>
using exponents_mod_t = typename exponents_mod<Exponents, Divisor>::type;

/**
 * Returns the quotients of a list of exponents divided by a divisor.
 *
 * @tparam Exponents
 *	The list of integer exponents
 * @tparam Divisor
 *	The divisor to divide by
 */
template <
    class Exponents, sint Divisor,
    class = enable_if_t<is_exponents_chain<Exponents>::value && Divisor != 0>>
struct exponents_div {
	/**
	 * Recursively divide all of the given exponents.
	 */
	typedef exponents_chain<
	    Exponents::value / Divisor,
	    typename exponents_div<typename Exponents::tail, Divisor>::type>
	    type;

	/**
	 * Indicate whether all the exponents were dividable by the divisor.
	 */
	static constexpr bool const exact{
	    is_same<exponents_mod_t<Exponents, Divisor>,
	            exponents_zero_t<Exponents>>::value};
};

/**
 * Terminates the recursion.
 *
 * @tparam Divisor
 *	The divisor to divide by
 * @see exponents_div
 */
template <sint Divisor>
struct exponents_div<void, Divisor> {
	/**
	 * The result list is void terminated.
	 */
	typedef void type;

	/**
	 * The empty list is always dividable.
	 */
	static constexpr bool const exact{true};
};

/**
 * Template alias for exponents_div<>::type.
 *
 * @tparam Exponents,Divisor
 *	The arguments to exponents_div
 */
template <class Exponents, sint Divisor>
using exponents_div_t = typename exponents_div<Exponents, Divisor>::type;

/*
 * Constant (non-rational) factors
 */

/**
 * Check whether the value member of a given type is static and assignable.
 *
 * This check also returns false if the type of the value member is not
 * default-constructible.
 *
 * @tparam Op
 *	The value container type
 */
template <class Op, class = void>
struct is_assignable : false_type {};

/**
 * Return true if ``Op::value = decltype(Op::value){}`` is valid code.
 *
 * @see is_assignable
 */
template <class Op>
struct is_assignable<Op, void_t<decltype(Op::value = decltype(Op::value){})>> :
       true_type {
};

/**
 * Test whether the given type is a container for a static non-assignable
 * value.
 *
 * This test can be tricked, but should be sufficient.
 *
 * The value needs to be convertible to a floating point value.
 *
 * @tparam Op
 *	The type to test
 */
template <class Op, class = void, class = void>
struct is_constant : false_type {};

/**
 * Return true if the given type non-assignable, static value member, that
 * can be cast to double.
 *
 * @see is_constant
 */
template <class Op>
struct is_constant<Op, enable_if_t<!is_assignable<Op>::value>,
                   void_t<decltype(double(Op::value))>> : true_type {};

/** @cond */
template <class Head, class Tail = void, class = void>
struct constants_chain;
/** @endcond */

/**
 * Test whether the given type is a constants chain type.
 *
 * @tparam Op
 *	The type to test
 */
template <class Op>
struct is_constants_chain : false_type {};

/**
 * Recursively validate the chain.
 *
 * @tparam Const,Tail
 *	The constant and the trailing constants_chain instant
 * @see is_constants_chain
 */
template <class Const, class Tail>
struct is_constants_chain<constants_chain<Const, Tail>>
    : is_constants_chain<Tail> {};

/**
 * Terminate the recursion.
 *
 * @see is_constants_chain
 */
template <>
struct is_constants_chain<void> : true_type {};

/**
 * A recursive chain of constant container types.
 *
 * @tparam Head
 *	The constant type in this chain link
 * @tparam Tail
 *	The attached constants_chain
 */
template <class Head, class Tail>
struct constants_chain /** @cond */<
    Head, Tail,
    enable_if_t<is_constant<Head>::value && is_constants_chain<Tail>::value>>
    /** @endcond */ {
	/**
	 * The tail of the chain of constant container types.
	 */
	typedef Tail tail;

	/**
	 * The current constant container type
	 */
	typedef Head type;
};

/**
 * A factory to create a constants_chain.
 *
 * @tparam Constants
 *	A list of constant container types
 */
template <class... Constants>
struct constants {
	/**
	 * Terminate the chain.
	 */
	typedef void type;
};

/**
 * Recursively create the constants_chain.
 *
 * @tparam Head
 *	The constant container type for the current chain link
 * @tparam Tail
 *	The remaining constant container types
 * @see constants
 */
template <class Head, class... Tail>
struct constants<Head, Tail...> {
	/**
	 * The constants_chain type.
	 */
	typedef constants_chain<Head, typename constants<Tail...>::type> type;
};

/**
 * A template alias for constants<>::type, producing a constants_chain.
 *
 * @tparam Args
 *	The arguments to pass on to constants
 */
template <class... Args>
using constants_t = typename constants<Args...>::type;

/**
 * Takes a constant value to a power.
 *
 * @tparam T
 *	The type to use to return the result
 * @tparam Constant
 *	The constant container type
 * @tparam Exponent
 *	The power to take the constant to
 */
template <typename T, class Constant, sint Exponent, bool = (Exponent > 0),
          class = enable_if_t<is_constant<Constant>::value>>
struct constant_pow {
	/**
	 * Recursively multiply the constant for positive exponents.
	 */
	static constexpr T const value{
	    T{Constant::value} *
	    constant_pow<T, Constant, Exponent - 1>::value};
};

/**
 * Recursion for negative exponents.
 *
 * @see constant_pow
 */
template <typename T, class Constant, sint Exponent>
struct constant_pow<T, Constant, Exponent, false> {
	/**
	 * Recursively divide by the constant value for negative exponents.
	 */
	static constexpr T const value{
	    T{1} / constant_pow<T, Constant, -Exponent>::value};
};

/**
 * Case for exponent 0.
 *
 * @see constant_pow
 */
template <typename T, class Constant>
struct constant_pow<T, Constant, 0, false> {
	/**
	 * Any constant to the power of 0 results in 1.
	 */
	static constexpr T const value{1};
};

/**
 * Provides a way to use floating-point constants in conversions.
 *
 * The recommended way to leverage this set of templates is to define an alias:
 *
 * ~~~~{.cpp}
 * struct constant_base10 { static constexpr double const value{10.};};
 * struct constant_base2 { static constexpr double const value{2.};};
 *
 * template <units::sint ... Exponents>
 * using constant_bases =
 *       units::constants_pack_t<units::constants_t<constant_base10,
 *                                                  constant_base2>,
 *                               Exponents ...>;
 * ~~~~
 *
 * The constants_pack_prod template can be used to extract the product
 * of all constants to the power of their exponents:
 *
 * ~~~~{.cpp}
 * typedef constants_bases<1, -1> base5;
 *
 * static_assert(constants_pack_prod<double, base5>::value == 5.,
 *               "The product of the base5 pack should be 5");
 * ~~~~
 *
 * @tparam Constants
 *	A set of constant container types
 * @tparam Exponents
 *	A set of integer exponents, one for each constant container type
 */
template <class Constants, class Exponents,
          class = enable_if_t<is_constants_chain<Constants>::value &&
                              is_exponents_chain<Exponents>::value &&
                              is_same_length<Constants, Exponents>::value>>
struct constants_pack {
	/**
	 * The constants_chain type.
	 */
	typedef Constants constants;

	/**
	 * The exponents_chain type.
	 */
	typedef Exponents exponents;
};

/**
 * Test whether a given type has the properties of a constants_pack.
 *
 * @tparam Op
 *	The type to check
 */
template <class Op>
struct is_constants_pack : false_type {};

/**
 * Return true for types that are constants_chain<> instances.
 *
 * @tparam Constants,Exponents
 *	The constants_chain and exponents_chain, that constitute a
 *      constants_pack
 * @see is_constants_pack
 */
template <class Constants, class Exponents>
struct is_constants_pack<constants_pack<Constants, Exponents>> : true_type {};

/**
 * A template alias for constants_pack<>.
 *
 * @tparam Constants
 *	A constants_chain
 * @tparam Exponents
 *	A list of signed integers for each constant container type
 */
template <class Constants, sint... Exponents>
using constants_pack_t = constants_pack<Constants, exponents_t<Exponents...>>;

/** @cond */
template <class>
struct constants_pack_unary;
/** @endcond */

/**
 * Unary operations on constant packs.
 *
 * @tparam Constants,Exponents
 *	The constants and exponents of the given pack
 */
template <class Constants, class Exponents>
struct constants_pack_unary<constants_pack<Constants, Exponents>> {
	/**
	 * A clone of the given constants_pack type with all exponents 0.
	 */
	typedef constants_pack<Constants, exponents_zero_t<Exponents>> neutral;

	/**
	 * A clone of the given constants_pack type with negated exponents.
	 */
	typedef constants_pack<Constants, exponents_negate_t<Exponents>>
	    invert;
};

/**
 * A template alias for constants_pack_unary<>::neutral.
 *
 * @tparam Args
 *	The arguments to pass on to constants_pack_unary
 */
template <class... Args>
using constants_pack_neutral_t =
    typename constants_pack_unary<Args...>::neutral;

/**
 * A template alias for constants_pack_unary<>::invert.
 *
 * @tparam Args
 *	The arguments to pass on to constants_pack_unary
 */
template <class... Args>
using constants_pack_invert_t = typename constants_pack_unary<Args...>::invert;

/**
 * Test whether two constants_pack instances are compatible for binary
 * operations.
 *
 * @tparam Lhs,Rhs
 *	The constants_pack types to compare
 */
template <class Lhs, class Rhs>
struct is_compatible_constants_pack : false_type {};

/**
 * Return true for operands that use the same constants_chain type.
 *
 * @tparam Constants
 *	The constants_chain, that needs to be identical for the Lhs and Rhs
 *	constants_pack instances
 * @tparam LExponents,RExponents
 *	The list of exponents_chain%s for the Lhs and Rhs constants_pack%s
 * @see is_compatible_constants_pack
 */
template <class Constants, class LExponents, class RExponents>
struct is_compatible_constants_pack<constants_pack<Constants, LExponents>,
                                    constants_pack<Constants, RExponents>>
    : true_type {};

/** @cond */
template <class, class>
struct constants_pack_binary;
/** @endcond */

/**
 * Binary operations on constants_pack types.
 *
 * @tparam Constants
 *	The constants_chain, that needs to be identical for the Lhs and Rhs
 *	constants_pack instances
 * @tparam LExponents,RExponents
 *	The list of exponents_chain%s for the Lhs and Rhs constants_pack%s
 */
template <class Constants, class LExponents, class RExponents>
struct constants_pack_binary<constants_pack<Constants, LExponents>,
                             constants_pack<Constants, RExponents>> {
	/**
	 * A derived constants_pack that is the result of pairwise
	 * multiplication of each constant.
	 *
	 * This is realised by using the pairwise sums of the operands'
	 * exponents.
	 */
	typedef constants_pack<Constants,
	                       exponents_add_t<LExponents, RExponents>> mul;
};

/**
 * A template alias for constants_pack_binary<>::mul.
 *
 * @tparam Args
 *	The arguments to pass on to constants_pack_binary
 */
template <class... Args>
using constants_pack_mul_t = typename constants_pack_binary<Args...>::mul;

/**
 * A template alias for constants_pack_binary<>::mul, that returns the
 * result of division.
 *
 * @tparam Lhs,Rhs
 *	The arguments to pass on to constants_pack_binary
 */
template <class Lhs, class Rhs>
using constants_pack_div_t =
    typename constants_pack_binary<Lhs, constants_pack_invert_t<Rhs>>::mul;

/**
 * Generates the product of all the constants to the powers of their
 * exponents.
 *
 * @tparam T
 *	The primitive (usually floating-point) type to return the product as
 * @tparam Constants,Exponents
 *	Equally sized sets of constants and exponents to build the product
 *	from
 */
template <typename T, class Constants, class Exponents,
          class = enable_if_t<is_constants_chain<Constants>::value &&
                              is_exponents_chain<Exponents>::value &&
                              is_same_length<Constants, Exponents>::value>>
struct constants_prod {
	/**
	 * Recursively generate the product of all constants to the power of
	 * their exponents.
	 */
	static constexpr T const value{
	    constant_pow<T, typename Constants::type,
	                 Exponents::value>::value *
	    constants_prod<T, typename Constants::tail,
	                   typename Exponents::tail>::value};
};

/**
 * Case for end of constants_chain.
 *
 * @see constants_prod
 */
template <typename T>
struct constants_prod<T, void, void> {
	/**
	 * Terminate the recursion.
	 */
	static constexpr T const value{1};
};

/** @cond */
template <typename, class>
struct constants_pack_prod;
/** @endcond */

/**
 * A wrapper around constants_prod, that takes a constants_pack.
 *
 * @tparam T
 *	The primitive type to return the product as
 * @tparam Constants,Exponents
 *	The constants_pack traits to call constants_prod with
 */
template <typename T, class Constants, class Exponents>
struct constants_pack_prod<T, constants_pack<Constants, Exponents>>
    : constants_prod<T, Constants, Exponents> {};

/*
 * UNIT
 */

/** @cond */
template <typename T, class BaseUnits, class Factor,
          class ConstantsPack = constants_pack<void, void>,
          class = enable_if_t<is_exponents_chain<BaseUnits>::value &&
                              is_rational<Factor>::value &&
                              is_constants_pack<ConstantsPack>::value>>
struct unit;
/** @endcond */

/**
 * Test whether a given type has the traits expected of a \ref unit.
 *
 * @tparam Op
 *	The type to validate
 */
template <class Op>
struct is_unit : false_type {};

/**
 * Return true if the given type is a \ref unit template instances.
 *
 * @tparam Args
 *	The template arguments to the \ref unit template
 * @see is_unit
 */
template <class... Args>
struct is_unit<unit<Args...>> : true_type {};

/** @cond */
template <class>
struct unit_unary;
/** @endcond */

/**
 * Unary operations on \ref unit%s.
 *
 * @tparam Op
 *	The unit type to provide unary operations for
 */
template <typename T, class BaseUnits, class Factor, class ConstantsPack>
struct unit_unary<unit<T, BaseUnits, Factor, ConstantsPack>> {
	/**
	 * A derived unit with all exponents 0.
	 */
	typedef unit<T, exponents_zero_t<BaseUnits>, rational<1, 1>,
	             constants_pack_neutral_t<ConstantsPack>> scalar;

	/**
	 * A derived unit with all exponents negated.
	 */
	typedef unit<T, exponents_negate_t<BaseUnits>,
	             rational_invert_t<Factor>,
	             constants_pack_invert_t<ConstantsPack>> invert;

	/**
	 * A derived unit representing the base units.
	 */
	typedef unit<T, BaseUnits, rational<1, 1>,
	             constants_pack_neutral_t<ConstantsPack>> base;
};

/**
 * A template alias for unit_unary<>::scalar.
 *
 * @tparam Args
 *	The arguments to pass on to unit_unary
 */
template <class... Args>
using unit_scalar_t = typename unit_unary<Args...>::scalar;

/**
 * A template alias for unit_unary<>::invert.
 *
 * @tparam Args
 *	The arguments to pass on to unit_unary
 */
template <class... Args>
using unit_invert_t = typename unit_unary<Args...>::invert;

/**
 * A template alias for unit_unary<>::base.
 *
 * @tparam Args
 *	The arguments to pass on to unit_unary
 */
template <class... Args>
using unit_base_t = typename unit_unary<Args...>::base;

/**
 * Checks whether the given types are compatible units.
 *
 * Compatible units must match the following criteria:
 * - They have the traits of a unit
 * - They encapsulate the same primitive type
 * - They have the same number of exponents
 * - They have the same set of constants
 *
 * @tparam Lhs,Rhs
 *	The units to check
 */
template <class Lhs, class Rhs, class = void>
struct is_compatible_unit : false_type {};

/**
 * Return true if the conditions for binary operations are met.
 *
 * @see is_compatible_unit
 */
template <class Lhs, class Rhs>
struct is_compatible_unit<
    Lhs, Rhs,
    enable_if_t<
        is_unit<Lhs>::value && is_unit<Rhs>::value &&
        is_same<typename Lhs::value_type, typename Rhs::value_type>::value &&
        is_same_length<typename Lhs::base_units,
                       typename Rhs::base_units>::value &&
        is_compatible_constants_pack<typename Lhs::constants_pack,
                                     typename Rhs::constants_pack>::value>>
    : true_type {};

/**
 * Binary operations for two unit types.
 *
 * @tparam Lhs,Rhs
 *	The unit type operands
 */
template <class Lhs, class Rhs,
          class = enable_if_t<is_compatible_unit<Lhs, Rhs>::value>>
struct unit_binary {
	/**
	 * Provides the type resulting from multiplying the given units.
	 */
	typedef unit<
	    typename Lhs::value_type,
	    exponents_add_t<typename Lhs::base_units,
	                    typename Rhs::base_units>,
	    rational_mul_t<typename Lhs::factor, typename Rhs::factor>,
	    constants_pack_mul_t<typename Lhs::constants_pack,
	                         typename Rhs::constants_pack>> mul;
};

/**
 * A template alias for unit_binary<>::mul.
 *
 * @tparam Args
 *	The arguments to pass to unit_binary
 */
template <class... Args>
using unit_mul_t = typename unit_binary<Args...>::mul;

/**
 * A template alias for unit_binary<>::div.
 *
 * @tparam Lhs,Rhs
 *	The arguments to pass to unit_binary
 */
template <class Lhs, class Rhs>
using unit_div_t = unit_mul_t<Lhs, unit_invert_t<Rhs>>;

/** @cond */
template <class Unit, sint Exponent, bool = (Exponent >= 0),
          class = enable_if_t<is_unit<Unit>::value>>
struct unit_pow;
/** @endcond */

/**
 * Takes a given unit to the power of an integer exponent.
 *
 * This is realised by recursively multiplying the given unit/value.
 *
 * @tparam Unit
 *	The Unit to take to a power
 * @tparam Exponent
 *	The exponent
 */
template <class Unit, sint Exponent>
struct unit_pow /** @cond */<Unit, Exponent, true> /** @endcond */ {
	/**
	 * The Unit to the power.
	 */
	typedef unit_mul_t<Unit, typename unit_pow<Unit, Exponent - 1>::type>
	    type;

	/**
	 * Takes an instance of Unit to the power of the Exponent.
	 *
	 * @param op
	 *	The unit instance
	 * @return
	 *	The unit to the power
	 */
	static constexpr type pow(Unit const op) {
		return op * unit_pow<Unit, Exponent - 1>::pow(op);
	}
};

/**
 * Any unit to the power 0 returns a scalar 1.
 *
 * @tparam Unit
 *	The unit to take to the power of 0
 * @see unit_pow
 */
template <class Unit>
struct unit_pow<Unit, 0> {
	/**
	 * The resulting type is the scalar type.
	 */
	typedef unit_scalar_t<Unit> type;

	/**
	 * Takes an instance of Unit to the power of 0.
	 *
	 * @param const
	 *	Everything to the power of 0 is a scalar 1
	 * @return
	 *	A scalar 1
	 */
	static constexpr type pow(Unit const) { return type{1}; }
};

/**
 * Takes a given unit to the power of a negative exponent.
 *
 * This works by inverting the result from the positive exponent case.
 *
 * @tparam Unit
 *	The unit instance
 * @tparam Exponent
 *	The exponent
 * @see unit_pow
 */
template <class Unit, sint Exponent>
struct unit_pow<Unit, Exponent, false> {
	/**
	 * The Unit to the power.
	 */
	typedef unit_invert_t<typename unit_pow<Unit, -Exponent>::type> type;

	/**
	 * Takes an instance of Unit to the power of the Exponent.
	 *
	 * @param op
	 *	The unit instance
	 * @return
	 *	The unit to the power
	 */
	static constexpr type pow(Unit const op) {
		return unit_scalar_t<Unit>{1} /
		       unit_pow<Unit, -Exponent>::pow(op);
	}
};

/**
 * A wrapper for unit_pow<>::type.
 *
 * @tparam Unit,Exponent
 *	The arguments to unit_pow
 */
template <class Unit, sint Exponent>
using unit_pow_t = typename unit_pow<Unit, Exponent>::type;

/**
 * Returns nth root of the given unit.
 *
 * Only works for units where the exponents are dividable by the degree.
 *
 * The returned unit always is a base unit.
 *
 * @tparam Unit
 *	The unit to take the root of
 * @tparam Degree
 *	The degree to which to take the root
 */
template <class Unit, sint Degree,
          class = enable_if_t<
              is_unit<Unit>::value &&
              exponents_div<typename Unit::base_units, Degree>::exact>>
struct unit_root {
	/**
	 * The unit resulting from taking the nth root of the given unit.
	 */
	typedef unit<
	    typename Unit::value_type,
	    exponents_div_t<typename Unit::base_units, Degree>, rational<1, 1>,
	    constants_pack_neutral_t<typename Unit::constants_pack>> type;
};

/**
 * A wrapper for unit_root<>::type.
 *
 * @tparam Unit,Degree
 *	The arguments to unit_root
 */
template <class Unit, sint Degree>
using unit_root_t = typename unit_root<Unit, Degree>::type;

/**
 * Test whether a given unit is a scalar unit type.
 *
 * @tparam Op
 *	The unit to test
 */
template <class Op, class = void>
struct is_unit_scalar : false_type {};

/**
 * Return whether the given type is identical to the derived scalar type.
 *
 * @tparam Op
 *	The unit to test
 * @see is_unit_scalar
 */
template <class Op>
struct is_unit_scalar<Op, void_t<unit_scalar_t<Op>>>
    : is_same<Op, unit_scalar_t<Op>> {};

/**
 * Test whether a given unit can be converted to the scalar unit type.
 *
 * @tparam Op
 *	The unit to test
 */
template <class Op, class = void>
struct has_unit_base_scalar : false_type {};

/**
 * Return whether the base of the operand unit is the scalar unit.
 *
 * @tparam Op
 *	The unit to test
 * @see has_unit_base_scalar
 */
template <class Op>
struct has_unit_base_scalar<Op, void_t<unit_base_t<Op>>>
    : is_same<unit_base_t<Op>, unit_scalar_t<Op>> {};

/**
 * A container for primitive types that gives them the properties of units.
 *
 * All units consist of a set of base units, each base unit is represented
 * by an exponent, so a scalar type has all exponents to the power of 0.
 *
 * The unit container provides type safety for these units, making sure
 * that where m are expected, m/s will cause compilation failure.
 *
 * Furthermore it provides conversion between different representations of
 * base units, provided they have a common 0 and can be converted using a
 * positive linear factor. E.g. inch can be provided for metres and will
 * transparently be converted. That works for most physical units, notable
 * exceptions are most temperature scales or units built on logarithmic scales.
 *
 * As many as possible of the functions provided by the cmath library are
 * supported. Some only for types convertable to the scalar type (e.g. exp(),
 * log(), pow()). Others work with any unit (e.g. hypot(), abs(), floor()).
 * These functions are mappings, so refer to the cmath documentation for
 * what they do.
 *
 * @tparam T
 *	The primitive (usually floating-point) type to encapsulate
 * @tparam BaseUnits
 *	An exponents_chain representing the base units
 * @tparam Factor
 *	A \ref rational linear conversion factor to the base units
 * @tparam ConstantsPack
 *	A constants_pack instance that can contain a set of real constants
 *	and exponents, to provide conversion factors that are not well
 *	representable as a rational value, such as \f$ \pi \f$
 * @see units_unit_cmath
 */
template <typename T, class BaseUnits, class Factor, class ConstantsPack>
struct unit /** @cond */<T, BaseUnits, Factor, ConstantsPack> /** @endcond */ {

	/**
	 * The encapsulated type.
	 */
	typedef T value_type;

	/**
	 * The exponents_chain representing the exponents of the base units.
	 */
	typedef BaseUnits base_units;

	/**
	 * The rational conversion factor.
	 */
	typedef Factor factor;

	/**
	 * The real conversion factors.
	 */
	typedef ConstantsPack constants_pack;

	/**
	 * The unit's own type.
	 */
	typedef unit<T, BaseUnits, Factor, ConstantsPack> type;

	/**
	 * The unit's base type.
	 */
	typedef unit_base_t<type> base;

	/**
	 * The encapsulated value.
	 */
	T value;

	/*
	 * Constructors
	 */

	/**
	 * The default constructor initialises with the value 0.
	 */
	constexpr unit() : value{0} {}

	/**
	 * The explicit copy constructor to create a unit from a primitive
	 * value.
	 *
	 * @param copy
	 *	The value to encapsulate
	 */
	constexpr explicit unit(T const copy) : value{copy} {}

	/**
	 * The implicit copy construct to create a scalar unit from a value.
	 *
	 * @tparam Implicit
	 *	True if the unit to be constructed is a scalar unit
	 * @param copy
	 *	The value to encapsulate
	 */
	template <bool Implicit = has_unit_base_scalar<type>::value,
	          class = enable_if_t<Implicit>>
	constexpr unit(T const copy)
	    : unit{base{copy}} {}

	/**
	 * Returns the \ref rational instance that represents the rational
	 * component of the conversion factor from the given unit to this unit.
	 *
	 * @tparam Op
	 *	The \ref unit to convert from
	 */
	template <class Op>
	using rational_factor_t = rational_div_t<typename Op::factor, Factor>;

	/**
	 * Returns the constants_pack representing the constants-based
	 * component of the conversion factor from the given unit to this unit.
	 *
	 * @tparam Op
	 *	The \ref unit to convert from
	 */
	template <class Op>
	using constants_div_t =
	    constants_pack_div_t<typename Op::constants_pack, ConstantsPack>;

	/**
	 * Returns the value container for the constants-based conversion
	 * factor from the given unit to this unit.
	 *
	 * @tparam Op
	 *	The \ref unit to convert from
	 */
	template <class Op>
	using constants_factor_t = constants_pack_prod<T, constants_div_t<Op>>;

	/**
	 * The convertible copy constructor converts the value of a compatible
	 * unit to match this unit.
	 *
	 * @tparam Op
	 *	The unit type to convert from
	 * @param copy
	 *	The unit typed value to convert from
	 */
	template <class Op,
	          class = enable_if_t<
	              is_compatible_unit<type, Op>::value &&
	              is_same<BaseUnits, typename Op::base_units>::value>>
	constexpr unit(Op const copy)
	    : value{copy.value * (T(rational_factor_t<Op>{}) *
	                          constants_factor_t<Op>::value)} {}

	/**
	 * Provides implicit copy initialisation.
	 *
	 * This is useful to explicitly construct the current type avoiding
	 * the use of explicit constructors. This enforces the use of the
	 * conversion constructors, e.g. to treat primitives as scalar values,
	 * instead of initialisers for the explicit constructor.
	 *
	 * @tparam Copy
	 *	The operand type
	 * @param copy
	 *	The operand
	 * @return
	 *	A copy of the operand converted to the type
	 */
	template <typename Copy>
	static constexpr type implicit(Copy const copy) {
		return copy;
	}

	/*
	 * Unary arithmetic operators
	 */

	/**
	 * The unary + operator returns a copy of this instance.
	 *
	 * @return
	 *	A copy of this instance
	 */
	constexpr type operator+() const { return *this; }

	/**
	 * The unary - operator returns a negated copy of this instance.
	 *
	 * @return
	 *	A copy of this instance, with its value negated
	 */
	constexpr type operator-() const { return type{-this->value}; }

	/*
	 * Binary arithmetic operators
	 */

	/**
	 * The binary + operator returns the sum of this value and the
	 * operand value.
	 *
	 * @param op
	 *	The operand
	 * @return
	 *	The sum
	 */
	constexpr type operator+(type const op) const {
		return type{this->value + op.value};
	}

	/**
	 * The binary - operator returns the difference between this value and
	 * the operand value.
	 *
	 * @param op
	 *	The operand
	 * @return
	 *	The difference
	 */
	constexpr type operator-(type const op) const {
		return type{this->value - op.value};
	}

	/**
	 * The binary * operator returns the product of this value and the
	 * operand value.
	 *
	 * @tparam Op
	 *	The operand type
	 * @tparam Result
	 *	The resulting type
	 * @param op
	 *	The operand
	 * @return
	 *	The product
	 */
	template <class Op, class Result = unit_mul_t<type, Op>>
	constexpr Result operator*(Op const op) const {
		return Result{this->value * op.value};
	}

	/**
	 * The binary * operator for scalar operands returns the product
	 * of this value and the operand value.
	 *
	 * @param op
	 *	The scalar operand
	 * @return
	 *	The product
	 */
	constexpr type operator*(T const op) const {
		return type{this->value * op};
	}

	/**
	 * The binary / operator returns the quotient of this value and the
	 * operand value.
	 *
	 * @tparam Op
	 *	The operand type
	 * @tparam Result
	 *	The resulting type
	 * @param op
	 *	The operand
	 * @return
	 *	The quotient
	 */
	template <class Op, class Result = unit_div_t<type, Op>>
	constexpr Result operator/(Op const op) const {
		return Result{this->value / op.value};
	}

	/**
	 * The binary / operator for scalar operands returns the quotient
	 * of this value and the operand value.
	 *
	 * @param op
	 *	The scalar operand
	 * @return
	 *	The quotient
	 */
	constexpr type operator/(T const op) const {
		return type{this->value / op};
	}

	/**
	 * Implementation of operator % relying on the operator being defined
	 * for the underlying type T.
	 *
	 * @tparam NativeMod
	 *	True if T has the module operator, used for enable_if
	 * @param op
	 *	The operand
	 * @return
	 *	The remainder
	 */
	template <bool NativeMod = has_op_modulo<T>::value>
	constexpr enable_if_t<NativeMod, type> operator%(type const op) const {
		return type{this->value % op.value};
	}

	/**
	 * Implementation of operator % relying on the presence of an
	 * fmod() function.
	 *
	 * @tparam NativeMod
	 *	True if T has the module operator, used for enable_if
	 * @param op
	 *	The operand
	 * @return
	 *	The remainder
	 */
	template <bool NativeMod = has_op_modulo<T>::value>
	constexpr enable_if_t<!NativeMod, type>
	operator%(type const op) const {
		using std::fmod; /* Match functions outside of struct scope. */
		return type{fmod(this->value, op.value)};
	}

	/*
	 * Arithmetic assignment
	 */

	/**
	 * The += assignment operator adds the operand value to this value.
	 *
	 * @param op
	 *	The operand
	 * @return
	 *	A self-reference
	 */
	type & operator+=(type const op) {
		this->value += op.value;
		return *this;
	}

	/**
	 * The -= assignment operator subtracts the operand value from this
	 * value.
	 *
	 * @param op
	 *	The operand
	 * @return
	 *	A self-reference
	 */
	type & operator-=(type const op) {
		this->value -= op.value;
		return *this;
	}

	/**
	 * The *= assignment operator multiplies this value with the scalar
	 * operand value.
	 *
	 * @param op
	 *	The operand
	 * @return
	 *	A self-reference
	 */
	type & operator*=(unit_scalar_t<type> const op) {
		this->value *= op.value;
		return *this;
	}

	/**
	 * The /= assignment operator divides this value by the scalar
	 * operand value.
	 *
	 * @param op
	 *	The operand
	 * @return
	 *	A self-reference
	 */
	type & operator/=(unit_scalar_t<type> const op) {
		this->value /= op.value;
		return *this;
	}

	/**
	 * Implementation of operator %= relying on the operator being defined
	 * for the underlying type T.
	 *
	 * @tparam NativeModAssign
	 *	True if T has the module assign operator, used for enable_if
	 * @param op
	 *	The operand
	 * @return
	 *	A self-reference
	 */
	template <bool NativeModAssign = has_op_assign_modulo<T>::value>
	enable_if_t<NativeModAssign, type &> operator%=(type const op) {
		this->value %= op.value;
		return *this;
	}

	/**
	 * Implementation of operator %= using operator %.
	 *
	 * @tparam NativeModAssign
	 *	True if T has the module assign operator, used for enable_if
	 * @param op
	 *	The operand
	 * @return
	 *	A self-reference
	 */
	template <bool NativeModAssign = has_op_assign_modulo<T>::value>
	enable_if_t<!NativeModAssign, type &> operator%=(type const op) {
		*this = *this % op;
		return *this;
	}

	/*
	 * Boolean operators
	 */

	/**
	 * The == operator compares this value with the operand value.
	 *
	 * @param op
	 *	The operand
	 * @return
	 *	The comparison result
	 */
	constexpr bool operator==(type const op) const {
		return this->value == op.value;
	}

	/**
	 * The != operator compares this value with the operand value.
	 *
	 * @param op
	 *	The operand
	 * @return
	 *	The comparison result
	 */
	constexpr bool operator!=(type const op) const {
		return this->value != op.value;
	}

	/**
	 * The < operator compares this value with the operand value.
	 *
	 * @param op
	 *	The operand
	 * @return
	 *	The comparison result
	 */
	constexpr bool operator<(type const op) const {
		return this->value < op.value;
	}

	/**
	 * The <= operator compares this value with the operand value.
	 *
	 * @param op
	 *	The operand
	 * @return
	 *	The comparison result
	 */
	constexpr bool operator<=(type const op) const {
		return this->value <= op.value;
	}

	/**
	 * The > operator compares this value with the operand value.
	 *
	 * @param op
	 *	The operand
	 * @return
	 *	The comparison result
	 */
	constexpr bool operator>(type const op) const {
		return this->value > op.value;
	}

	/**
	 * The >= operator compares this value with the operand value.
	 *
	 * @param op
	 *	The operand
	 * @return
	 *	The comparison result
	 */
	constexpr bool operator>=(type const op) const {
		return this->value >= op.value;
	}

	/*
	 * Friend operators, deal with left hand primitives that should
	 * be treated as scalar unit instances.
	 */

	/**
	 * Binary * operator for primitives as a scalar unit.
	 *
	 * @param lhs,rhs
	 *	The primitive and the unit operands
	 * @return
	 *	The product of the primitive and the scalar unit
	 */
	friend constexpr type operator*(T const lhs, type const rhs) {
		return type{lhs * rhs.value};
	}

	/**
	 * Binary / operator for primitives as a scalar unit.
	 *
	 * @param lhs,rhs
	 *	The primitive and the unit operands
	 * @return
	 *	The quotient of the primitive and the scalar unit
	 */
	friend constexpr unit_invert_t<type> operator/(T const lhs,
	                                               type const rhs) {
		return unit_invert_t<type>{lhs / rhs.value};
	}

	/**
	 * Binary + operator for primitives as a scalar unit.
	 *
	 * Only works if the rhs value is a scalar unit.
	 *
	 * @tparam Scalar
	 *	True if type is scalar, used in enable_if
	 * @param lhs,rhs
	 *	The primitive and the unit operands
	 * @return
	 *	The sum of the primitive and the scalar unit
	 */
	template <bool Scalar = has_unit_base_scalar<type>::value>
	friend constexpr enable_if_t<Scalar, base> operator+(T const lhs,
	                                                     type const rhs) {
		return base{lhs + base{rhs}.value};
	}

	/**
	 * Binary - operator for primitives as a scalar unit.
	 *
	 * Only works if the rhs value is a scalar unit.
	 *
	 * @tparam Scalar
	 *	True if type is scalar, used in enable_if
	 * @param lhs,rhs
	 *	The primitive and the unit operands
	 * @return
	 *	The difference between the primitive and the scalar unit
	 */
	template <bool Scalar = has_unit_base_scalar<type>::value>
	friend constexpr enable_if_t<Scalar, base> operator-(T const lhs,
	                                                     type const rhs) {
		return base{lhs - base{rhs}.value};
	}

	/**
	 * Binary % operator for primitives as a scalar unit.
	 *
	 * Only works if the rhs value is a scalar unit.
	 *
	 * This is the implementation for types with a native % operator, which
	 * is assumed to be constexpr.
	 *
	 * @tparam Scalar
	 *	True if type is scalar, used in enable_if
	 * @param lhs,rhs
	 *	The primitive and the unit operands
	 * @return
	 *	The remainder of the primitive and the scalar unit
	 */
	template <bool Scalar = has_unit_base_scalar<type>::value>
	friend constexpr enable_if_t<Scalar, base> operator%(T const lhs,
	                                                     type const rhs) {
		return base{lhs} % rhs;
	}

	/**
	 * Binary == operator for primitives as a scalar unit.
	 *
	 * @tparam Scalar
	 *	True if type is scalar, used in enable_if
	 * @param lhs,rhs
	 *	The primitive and the unit operands
	 * @return
	 *	The comparison result
	 */
	template <bool Scalar = has_unit_base_scalar<type>::value>
	friend constexpr enable_if_t<Scalar, bool> operator==(T const lhs,
	                                                      type const rhs) {
		return base{rhs} == lhs;
	}

	/**
	 * Binary != operator for primitives as a scalar unit.
	 *
	 * @tparam Scalar
	 *	True if type is scalar, used in enable_if
	 * @param lhs,rhs
	 *	The primitive and the unit operands
	 * @return
	 *	The comparison result
	 */
	template <bool Scalar = has_unit_base_scalar<type>::value>
	friend constexpr enable_if_t<Scalar, bool> operator!=(T const lhs,
	                                                      type const rhs) {
		return base{rhs} != lhs;
	}

	/**
	 * Binary < operator for primitives as a scalar unit.
	 *
	 * @tparam Scalar
	 *	True if type is scalar, used in enable_if
	 * @param lhs,rhs
	 *	The primitive and the unit operands
	 * @return
	 *	The comparison result
	 */
	template <bool Scalar = has_unit_base_scalar<type>::value>
	friend constexpr enable_if_t<Scalar, bool> operator<(T const lhs,
	                                                     type const rhs) {
		return base{rhs} > lhs;
	}

	/**
	 * Binary <= operator for primitives as a scalar unit.
	 *
	 * @tparam Scalar
	 *	True if type is scalar, used in enable_if
	 * @param lhs,rhs
	 *	The primitive and the unit operands
	 * @return
	 *	The comparison result
	 */
	template <bool Scalar = has_unit_base_scalar<type>::value>
	friend constexpr enable_if_t<Scalar, bool> operator<=(T const lhs,
	                                                      type const rhs) {
		return base{rhs} >= lhs;
	}

	/**
	 * Binary > operator for primitives as a scalar unit.
	 *
	 * @tparam Scalar
	 *	True if type is scalar, used in enable_if
	 * @param lhs,rhs
	 *	The primitive and the unit operands
	 * @return
	 *	The comparison result
	 */
	template <bool Scalar = has_unit_base_scalar<type>::value>
	friend constexpr enable_if_t<Scalar, bool> operator>(T const lhs,
	                                                     type const rhs) {
		return base{rhs} < lhs;
	}

	/**
	 * Binary >= operator for primitives as a scalar unit.
	 *
	 * @tparam Scalar
	 *	True if type is scalar, used in enable_if
	 * @param lhs,rhs
	 *	The primitive and the unit operands
	 * @return
	 *	The comparison result
	 */
	template <bool Scalar = has_unit_base_scalar<type>::value>
	friend constexpr enable_if_t<Scalar, bool> operator>=(T const lhs,
	                                                      type const rhs) {
		return base{rhs} <= lhs;
	}

	/*
	 * These functions are member functions, to mark their close
	 * conceptual relationship to units.
	 */

	/**
	 * Takes this unit and value to an integer power.
	 *
	 * Available for all units and guaranteed constexpr, relies on
	 * recursive multiplication and division through unit_pow.
	 *
	 * @tparam Exponent
	 *	The exponent
	 * @return
	 *	The value to the given power with the unit to the given power
	 */
	template <sint Exponent>
	constexpr unit_pow_t<type, Exponent> pow() const {
		return unit_pow<type, Exponent>::pow(*this);
	}

	/**
	 * Returns the nth root of this unit and value.
	 *
	 * Relies on std::pow() for the value.
	 *
	 * The returned unit always is a base unit.
	 *
	 * This function is only available if all the base unit exponents
	 * are dividable by the exponent.
	 *
	 * @tparam Root
	 *	The root index
	 * @return
	 *	The nth root of the given value
	 */
	template <sint Root>
	constexpr unit_root_t<base, Root> root() const {
		using std::pow;
		return unit_root_t<base, Root>{
		    pow(base{*this}.value, T{1} / T{Root})};
	}

	/**
	 * Returns this unit and value to a rational power.
	 *
	 * Relies on std::pow() for the value.
	 *
	 * The returned unit always is a base unit.
	 *
	 * This function is only available if all the base unit exponents
	 * are dividable by the exponent.
	 *
	 * @tparam ExpNum,ExpDenom
	 *	The rational power
	 * @return
	 *	The value to the power
	 */
	template <sint ExpNum, sint ExpDenom>
	constexpr unit_root_t<unit_pow_t<base, ExpNum>, ExpDenom> pow() const {
		using std::pow;
		return unit_root_t<unit_pow_t<base, ExpNum>, ExpDenom>{
		    pow(base{*this}.value, T{ExpNum} / T{ExpDenom})};
	}

	/** @{ */
	/**
	 * @defgroup units_unit_cmath \<cmath\> Functions
	 *
	 * Provides the \<cmath\> functionality that is applicable to \ref unit
	 * typed values.
	 *
	 * @{
	 */

	/*
	 * Exponential and logarithmic functions from <cmath>
	 */

	template <bool Scalar = has_unit_base_scalar<type>::value>
	friend constexpr enable_if_t<Scalar, base> exp(type const x) {
		using std::exp;
		return base{exp(base{x}.value)};
	}

	friend constexpr type frexp(type const x, int * const exp) {
		using std::frexp;
		return type{frexp(x.value, exp)};
	}

	friend constexpr type ldexp(type const x, int const exp) {
		using std::ldexp;
		return type{ldexp(x.value, exp)};
	}

	template <bool Scalar = has_unit_base_scalar<type>::value>
	friend constexpr enable_if_t<Scalar, base> log(type const x) {
		using std::log;
		return base{log(base{x}.value)};
	}

	template <bool Scalar = has_unit_base_scalar<type>::value>
	friend constexpr enable_if_t<Scalar, base> log10(type const x) {
		using std::log10;
		return base{log10(base{x}.value)};
	}

	friend constexpr type modf(type const x, type & intpart) {
		using std::modf;
		return type{modf(x.value, &intpart.value)};
	}

	template <bool Scalar = has_unit_base_scalar<type>::value>
	friend constexpr enable_if_t<Scalar, base> exp2(type const x) {
		using std::exp2;
		return base{exp2(base{x}.value)};
	}

	template <bool Scalar = has_unit_base_scalar<type>::value>
	friend constexpr enable_if_t<Scalar, base> expm1(type const x) {
		using std::expm1;
		return base{expm1(base{x}.value)};
	}

	template <bool Scalar = has_unit_base_scalar<type>::value>
	friend constexpr enable_if_t<Scalar, base> log1p(type const x) {
		using std::log1p;
		return base{log1p(base{x}.value)};
	}

	template <bool Scalar = has_unit_base_scalar<type>::value>
	friend constexpr enable_if_t<Scalar, base> log2(type const x) {
		using std::log2;
		return base{log2(base{x}.value)};
	}

	template <bool Scalar = has_unit_base_scalar<type>::value>
	friend constexpr enable_if_t<Scalar, base> logb(type const x) {
		using std::logb;
		return base{logb(base{x}.value)};
	}

	friend constexpr type scalbn(type const x, int const n) {
		using std::scalbn;
		return type{scalbn(x.value, n)};
	}

	friend constexpr type scalbln(type const x, long const n) {
		using std::scalbln;
		return type{scalbln(x.value, n)};
	}

	/*
	 * Power functions from <cmath>
	 */

	template <bool Scalar = has_unit_base_scalar<type>::value>
	friend constexpr enable_if_t<Scalar, base> pow(type const x,
	                                               T const exp) {
		using std::pow;
		return base{pow(base{x}.value, exp)};
	}

	template <class Unit = type>
	friend constexpr unit_root_t<Unit, 2> sqrt(type const x) {
		using std::sqrt;
		return unit_root_t<Unit, 2>{sqrt(base{x}.value)};
	}

	template <class Unit = type>
	friend constexpr unit_root_t<Unit, 3> cbrt(type const x) {
		using std::cbrt;
		return unit_root_t<Unit, 3>{cbrt(base{x}.value)};
	}

	template <class Unit, class To = type, class = decltype(To{} + Unit{})>
	friend constexpr type hypot(type const x, Unit const y) {
		using std::hypot;
		return type{hypot(x.value, implicit(y).value)};
	}

	/*
	 * Rounding and remainder functions from <cmath>
	 */

	friend constexpr type ceil(type const x) {
		using std::ceil;
		return type{ceil(x.value)};
	}

	friend constexpr type floor(type const x) {
		using std::floor;
		return type{floor(x.value)};
	}

	template <class Unit, class To = type, class = decltype(To{} + Unit{})>
	friend constexpr type fmod(type const numer, Unit const denom) {
		using std::fmod;
		return type{fmod(numer.value, implicit(denom).value)};
	}

	friend constexpr type trunc(type const x) {
		using std::trunc;
		return type{trunc(x.value)};
	}

	friend constexpr type round(type const x) {
		using std::round;
		return type{round(x.value)};
	}

	friend constexpr type rint(type const x) {
		using std::rint;
		return type{rint(x.value)};
	}

	friend constexpr type nearbyint(type const x) {
		using std::nearbyint;
		return type{nearbyint(x.value)};
	}

	template <class Unit, class To = type, class = decltype(To{} + Unit{})>
	friend constexpr type remainder(type const numer, Unit const denom) {
		using std::remainder;
		return type{remainder(numer.value, implicit(denom).value)};
	}

	template <class Unit, class To = type, class = decltype(To{} + Unit{})>
	friend constexpr type remquo(type const numer, Unit const denom,
	                             int * const quot) {
		using std::remquo;
		return type{remquo(numer.value, implicit(denom).value, quot)};
	}

	/*
	 * Floating-point manipulation functions from <cmath>
	 */

	template <class Unit, class To = type, class = decltype(To{} + Unit{})>
	friend constexpr type copysign(type const x, Unit const y) {
		using std::copysign;
		return type{copysign(x.value, implicit(y).value)};
	}

	template <class Unit, class To = type, class = decltype(To{} + Unit{})>
	friend constexpr type nextafter(type const x, Unit const y) {
		using std::nextafter;
		return type{nextafter(x.value, implicit(y).value)};
	}

	/*
	 * Minimum, maximum, difference functions from <cmath>
	 */

	template <class Unit, class To = type, class = decltype(To{} + Unit{})>
	friend constexpr type fdim(type const x, Unit const y) {
		using std::fdim;
		return type{fdim(x.value, implicit(y).value)};
	}

	template <class Unit, class To = type, class = decltype(To{} + Unit{})>
	friend constexpr type fmax(type const x, Unit const y) {
		using std::fmax;
		return type{fmax(x.value, implicit(y).value)};
	}

	template <class Unit, class To = type, class = decltype(To{} + Unit{})>
	friend constexpr type fmin(type const x, Unit const y) {
		using std::fmin;
		return type{fmin(x.value, implicit(y).value)};
	}

	/*
	 * Other functions from <cmath>
	 */

	friend constexpr type fabs(type const x) {
		using std::fabs;
		return type{fabs(x.value)};
	}

	friend constexpr type abs(type const x) {
		using std::abs;
		return type{abs(x.value)};
	}

	template <class Unit>
	friend constexpr unit_mul_t<type, Unit>
	fma(type const x, Unit const y, unit_mul_t<type, Unit> const z) {
		using std::fma;
		return unit_mul_t<type, Unit>{fma(x.value, y.value, z.value)};
	}

	template <class Unit, class To = type, class = decltype(To{} + Unit{})>
	friend constexpr type fma(type const x, T const y, Unit const z) {
		using std::fma;
		return type{fma(x.value, y, implicit(z).value)};
	}

	template <class Unit, class To = type, class = decltype(To{} + Unit{})>
	friend constexpr type fma(T const x, type const y, Unit const z) {
		using std::fma;
		return type{fma(x, y.value, implicit(z).value)};
	}

	/**
	 * @}
	 */
	/** @} */
};

/*
 * Validation
 */

/**
 * Validates whether the given units constitute a compatible set of unit
 * template instances.
 *
 * The counter allows finding the offending type in the diagnostic output.
 *
 * @tparam Count
 *	A running count of the types to check, to ease finding offending
 *	types in diagnostic output
 * @tparam Units
 *	A list of units
 */
template <int Count, class... Units>
struct validate_units_count;

/**
 * Recursively validate unit pairs.
 *
 * @tparam Count
 *	The position of Lhs in the list of units
 * @tparam Lhs,Rhs
 *	The units to check for compatibility
 * @tparam Rhs,Units
 *	The units to recursively check for compatibility
 */
template <int Count, class Lhs, class Rhs, class... Units>
struct validate_units_count<Count, Lhs, Rhs, Units...>
    : validate_units_count<Count + 1, Rhs, Units...> {

	static_assert(is_unit<Lhs>::value,
	              "The current given type is not a valid unit template "
	              "instance");

	static_assert(is_compatible_unit<Lhs, Rhs>::value,
	              "The given types are not compatible units");
};

/**
 * Validate a single unit.
 *
 * @tparam Count
 *	The position of Lhs in the list of units
 * @tparam Unit
 *	An unpaired unit, can only be checked for being a valid unit
 */
template <int Count, class Unit>
struct validate_units_count<Count, Unit> {

	static_assert(is_unit<Unit>::value,
	              "The given type is not a valid unit template instance");

	/**
	 * The number of types checked.
	 */
	static constexpr int value{Count};
};

/**
 * Validate the given list of units.
 *
 * Uses validate_units_count, starting the count with 1.
 *
 * @tparam Units
 *	The unit types to pass on to validate_units_count
 */
template <class... Units>
struct validate_units : validate_units_count<1, Units...> {};

} /* namespace units */

#endif /* _UNITS_UNITS_HPP_ */
