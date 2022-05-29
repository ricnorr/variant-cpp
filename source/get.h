#pragma once
#include "storage.h"

template <class Visitor, class... Variants>
constexpr decltype(auto) visit_indexed(Visitor&& vis, Variants&&... variants);

template <class Visitor, class... Variants>
constexpr decltype(auto) visit_typed(Visitor&& vis, Variants&&... variants);

template <std::size_t I, class... Types>
constexpr variant_alternative_t<I, variant<Types...>>& get(variant<Types...>& v) {
  static_assert(sizeof...(Types) > I);
  if (v.valueless_by_exception() || I != v.index()) {
    throw bad_variant_access();
  }
  return v.storage.template get_storage<I>().value;
}

template< std::size_t I, class... Types >
constexpr const variant_alternative_t<I, variant<Types...>>&
get( const variant<Types...>& v ) {
  static_assert(sizeof...(Types) > I);
  if (v.valueless_by_exception() || I != v.index()) {
    throw bad_variant_access();
  }
  return v.storage.template get_storage<I>().value;
}

template <std::size_t I, class... Types>
constexpr variant_alternative_t<I, variant<Types...>>&& get(variant<Types...>&& v) {
  static_assert(sizeof...(Types) > I);
  if (v.valueless_by_exception() || I != v.index()) {
    throw bad_variant_access();
  }
  return std::move(v.storage.template get_storage<I>().value);
}


template <std::size_t I, class... Types>
constexpr const variant_alternative_t<I, variant<Types...>>&& get(const variant<Types...>&& v) {
  static_assert(sizeof...(Types) > I);
  if (v.valueless_by_exception() || I != v.index()) {
    throw bad_variant_access();
  }
  return std::move(v.storage.template get_storage<I>().value);
}

template <class T, class... Types>
constexpr T& get(variant<Types...>& v) {
  static_assert(count_type<T, Types...>::count == 1);
  return get<find_type_index<T, Types...>::count>(v);
}

template< class T, class... Types >
constexpr T&& get(variant<Types...>&& v ) {
  static_assert(count_type<T, Types...>::count == 1);
  return std::move(get<find_type_index<T, Types...>::count>(v));
}

template< class T, class... Types >
constexpr const T& get( const variant<Types...>& v ) {
  static_assert(count_type<T, Types...>::count == 1);
  return get<find_type_index<T, Types...>::count>(v);
}

template< class T, class... Types >
constexpr const T&& get( const variant<Types...>&& v ) {
  static_assert(count_type<T, Types...>::count == 1);
  return std::move(get<find_type_index<T, Types...>::count>(v));
}

template <std::size_t I, class... Types>
constexpr std::add_pointer_t<variant_alternative_t<I, variant<Types...>>> get_if(variant<Types...>* pv) noexcept {
  if (pv == nullptr || pv->index() != I) {
    return nullptr;
  }
  return std::addressof(get<I>(*pv));
}

template <std::size_t I, class... Types>
constexpr std::add_pointer_t<const variant_alternative_t<I, variant<Types...>>>
get_if(const variant<Types...>* pv) noexcept {
  if (pv == nullptr || pv->index() != I) {
    return nullptr;
  }
  return std::addressof(get<I>(*pv));
}

template <class T, class... Types>
constexpr std::add_pointer_t<T> get_if(variant<Types...>* pv) noexcept {
  if (pv == nullptr || !(holds_alternative<T>(*pv))) {
    return nullptr;
  }
  return std::addressof(get<T>(*pv));
}

template <class T, class... Types>
constexpr std::add_pointer_t<const T> get_if(const variant<Types...>* pv) noexcept {
  if (pv == nullptr || !(holds_alternative<T>(*pv))) {
    return nullptr;
  }
  return std::addressof(get<T>(*pv));
}
