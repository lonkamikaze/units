/** \file
 * Implements SFINAE constructs
 */

#ifndef _TESTS_SFINAE_HPP_
#define _TESTS_SFINAE_HPP_

#include <type_traits> /* std::true_type, std::false_type */

/**
 * This namespace provides boilerplate for test cases. Types tested must
 * be default constructible.
 *
 * The contained tests test whether a certain operation has a deducible type,
 * by way of SFINAE (substitution failure is not an error).
 *
 * They can be used to statically check whether:
 * - Operations would compile (even non-constexpr operations)
 * - Illegal operations would fail to compile due to non-resolution
 *
 * They cannot be used to check whether:
 * - A static_assert is triggered
 * - An operation can be performed correctly
 */
namespace tests {

/**
 * A common idiom, void_t instances with valid template arguments result
 * in the void type, otherwise they trigger SFINAE.
 */
template <class ...>
using void_t = void;

template <typename, class = void>
struct construct_default : std::false_type {
};

/**
 * Determine whether a type is default constructible.
 *
 * @tparam T
 *	The type to construct
 */
template <typename T>
struct construct_default<T, void_t<decltype(T{})>> : std::true_type {
};

template <typename, typename, class = void>
struct construct_copy : std::false_type {
};

/**
 * Determine whether one type can be constructed from another.
 *
 * Note this invokes a move constructor if present, copy otherwise.
 *
 * @tparam To
 *	The type to construct
 * @tparam From
 *	The type to construct from
 */
template <typename To, typename From>
struct construct_copy<To, From, void_t<decltype(To{From{}})>> : std::true_type {
};

/**
 * Turns a temporary into a const reference to block move construction.
 *
 * @tparam T
 *	The type to return a const reference to
 * @param t
 *	The instance to const reference to
 * @return
 *	A const reference to t
 */
template <typename T> constexpr T const & identity(T const & t) {
	return t;
}

template <typename, typename, class = void>
struct construct_forcecopy : std::false_type {
};

/**
 * Determine whether one type can be copy constructed from another.
 *
 * This forces copy construction, even when move construction is available.
 *
 * @tparam To
 *	The type to construct
 * @tparam From
 *	The type to construct from
 */
template <typename To, typename From>
struct construct_forcecopy<To, From, void_t<decltype(To{identity(From{})})>> :
       std::true_type {
};

template <typename, typename, class = void>
struct op_assign : std::false_type {
};

/**
 * Check whether types support assignment.
 *
 * @tparam Lhs
 *	Left hand side type
 * @tparam Rhs
 *	Right hand side type
 */
template <typename Lhs, typename Rhs>
struct op_assign<Lhs, Rhs, void_t<decltype(Lhs{} = Rhs{})>> : std::true_type {
};

template <typename, typename, class = void>
struct op_assign_plus : std::false_type {
};

/**
 * Check whether types support addition assignment.
 *
 * @tparam Lhs
 *	Left hand side type
 * @tparam Rhs
 *	Right hand side type
 */
template <typename Lhs, typename Rhs>
struct op_assign_plus<Lhs, Rhs, void_t<decltype(Lhs{} += Rhs{})>> : std::true_type {
};

template <typename, typename, class = void>
struct op_assign_minus : std::false_type {
};

/**
 * Check whether types support subtraction assignment.
 *
 * @tparam Lhs
 *	Left hand side type
 * @tparam Rhs
 *	Right hand side type
 */
template <typename Lhs, typename Rhs>
struct op_assign_minus<Lhs, Rhs, void_t<decltype(Lhs{} -= Rhs{})>> : std::true_type {
};

template <typename, typename, class = void>
struct op_assign_multiply : std::false_type {
};

/**
 * Check whether types support multiplication assignment.
 *
 * @tparam Lhs
 *	Left hand side type
 * @tparam Rhs
 *	Right hand side type
 */
template <typename Lhs, typename Rhs>
struct op_assign_multiply<Lhs, Rhs, void_t<decltype(Lhs{} *= Rhs{})>> : std::true_type {
};

template <typename, typename, class = void>
struct op_assign_divide : std::false_type {
};

/**
 * Check whether types support division assignment.
 *
 * @tparam Lhs
 *	Left hand side type
 * @tparam Rhs
 *	Right hand side type
 */
template <typename Lhs, typename Rhs>
struct op_assign_divide<Lhs, Rhs, void_t<decltype(Lhs{} /= Rhs{})>> : std::true_type {
};

template <typename, typename, class = void>
struct op_assign_modulo : std::false_type {
};

/**
 * Check whether types support modulo assignment.
 *
 * @tparam Lhs
 *	Left hand side type
 * @tparam Rhs
 *	Right hand side type
 */
template <typename Lhs, typename Rhs>
struct op_assign_modulo<Lhs, Rhs, void_t<decltype(Lhs{} %= Rhs{})>> : std::true_type {
};

template <typename, typename, class = void>
struct op_plus : std::false_type {
};

/**
 * Check whether types support addition.
 *
 * @tparam Lhs
 *	Left hand side type
 * @tparam Rhs
 *	Right hand side type
 */
template <typename Lhs, typename Rhs>
struct op_plus<Lhs, Rhs, void_t<decltype(Lhs{} + Rhs{})>> : std::true_type {
};

template <typename, typename, class = void>
struct op_minus : std::false_type {
};

/**
 * Check whether types support subtraction.
 *
 * @tparam Lhs
 *	Left hand side type
 * @tparam Rhs
 *	Right hand side type
 */
template <typename Lhs, typename Rhs>
struct op_minus<Lhs, Rhs, void_t<decltype(Lhs{} - Rhs{})>> : std::true_type {
};

template <typename, typename, class = void>
struct op_multiply : std::false_type {
};

/**
 * Check whether types support multiplication.
 *
 * @tparam Lhs
 *	Left hand side type
 * @tparam Rhs
 *	Right hand side type
 */
template <typename Lhs, typename Rhs>
struct op_multiply<Lhs, Rhs, void_t<decltype(Lhs{} * Rhs{})>> : std::true_type {
};

template <typename, typename, class = void>
struct op_divide : std::false_type {
};

/**
 * Check whether types support division.
 *
 * @tparam Lhs
 *	Left hand side type
 * @tparam Rhs
 *	Right hand side type
 */
template <typename Lhs, typename Rhs>
struct op_divide<Lhs, Rhs, void_t<decltype(Lhs{} / Rhs{})>> : std::true_type {
};

template <typename, typename, class = void>
struct op_modulo : std::false_type {
};

/**
 * Check whether types support modulo.
 *
 * @tparam Lhs
 *	Left hand side type
 * @tparam Rhs
 *	Right hand side type
 */
template <typename Lhs, typename Rhs>
struct op_modulo<Lhs, Rhs, void_t<decltype(Lhs{} % Rhs{})>> : std::true_type {
};

template <typename, typename, class = void>
struct op_equal : std::false_type {
};

/**
 * Check whether types support equal comparison.
 *
 * @tparam Lhs
 *	Left hand side type
 * @tparam Rhs
 *	Right hand side type
 */
template <typename Lhs, typename Rhs>
struct op_equal<Lhs, Rhs, void_t<decltype(Lhs{} == Rhs{})>> : std::true_type {
};

template <typename, typename, class = void>
struct op_unequal : std::false_type {
};

/**
 * Check whether types support unequal comparison.
 *
 * @tparam Lhs
 *	Left hand side type
 * @tparam Rhs
 *	Right hand side type
 */
template <typename Lhs, typename Rhs>
struct op_unequal<Lhs, Rhs, void_t<decltype(Lhs{} != Rhs{})>> : std::true_type {
};

template <typename, typename, class = void>
struct op_greater : std::false_type {
};

/**
 * Check whether types support greater than comparison.
 *
 * @tparam Lhs
 *	Left hand side type
 * @tparam Rhs
 *	Right hand side type
 */
template <typename Lhs, typename Rhs>
struct op_greater<Lhs, Rhs, void_t<decltype(Lhs{} > Rhs{})>> : std::true_type {
};

template <typename, typename, class = void>
struct op_greater_equal : std::false_type {
};

/**
 * Check whether types support greater than or equal comparison.
 *
 * @tparam Lhs
 *	Left hand side type
 * @tparam Rhs
 *	Right hand side type
 */
template <typename Lhs, typename Rhs>
struct op_greater_equal<Lhs, Rhs, void_t<decltype(Lhs{} >= Rhs{})>> : std::true_type {
};

template <typename, typename, class = void>
struct op_less : std::false_type {
};

/**
 * Check whether types support less than comparison.
 *
 * @tparam Lhs
 *	Left hand side type
 * @tparam Rhs
 *	Right hand side type
 */
template <typename Lhs, typename Rhs>
struct op_less<Lhs, Rhs, void_t<decltype(Lhs{} < Rhs{})>> : std::true_type {
};

template <typename, typename, class = void>
struct op_less_equal : std::false_type {
};

/**
 * Check whether types support less than or equal comparison.
 *
 * @tparam Lhs
 *	Left hand side type
 * @tparam Rhs
 *	Right hand side type
 */
template <typename Lhs, typename Rhs>
struct op_less_equal<Lhs, Rhs, void_t<decltype(Lhs{} <= Rhs{})>> : std::true_type {
};

} /* namespace tests */

#endif /* _TESTS_SFINAE_HPP_ */

