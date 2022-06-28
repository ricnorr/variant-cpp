#include "variant_impl.h"

template<class... T>
struct variant;

template <class... Types>
constexpr bool operator==(const variant<Types...>& v, const variant<Types...>& w) {
  if (v.index() != w.index()) {
    return false;
  }
  if (v.valueless_by_exception()) {
    return true;
  }
  return visit_indexed(
      [&v, &w]<size_t L, size_t R>(in_place_index_t<L>, in_place_index_t<R>) {
        if constexpr (L == R) {
          return get<L>(v) == get<R>(w);
        }
        { return false; }
      },
      v, w);
}

template< class... Types >
constexpr bool operator!=( const variant<Types...>& v,
                          const variant<Types...>& w ) {
  return !(v == w);
}

template< class... Types >
constexpr bool operator<( const variant<Types...>& v,
                         const variant<Types...>& w ) {
  if (w.valueless_by_exception()) {
    return false;
  }
  if (v.valueless_by_exception()) {
    return true;
  }
  if (v.index() < w.index()) {
    return true;
  }
  if (v.index() > w.index()) {
    return false;
  }
  return visit_indexed(
      [&v, &w]<size_t L, size_t R>(in_place_index_t<L>, in_place_index_t<R>) {
        if constexpr (L == R) {
          return get<L>(v) < get<R>(w);
        }
        { return false; }
      },
      v, w);
}

template< class... Types >
constexpr bool operator>( const variant<Types...>& v,
                         const variant<Types...>& w ) {
  return !(v == w) && !(v < w);
}

template< class... Types >
constexpr bool operator<=( const variant<Types...>& v,
                          const variant<Types...>& w ) {
  return !(v > w);
}

template< class... Types >
constexpr bool operator>=( const variant<Types...>& v,
                          const variant<Types...>& w ) {
  return !(v < w);
}

