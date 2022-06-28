#pragma once
#include "get.h"
#include <array>



constexpr decltype(auto) at(auto& array) {
  return array;
}


template <typename Array, typename... Is>
constexpr auto& at(Array& array, size_t i, Is... is) {
  if (i == variant_npos) {
    throw bad_variant_access();
  }
  return at(array[i], is...);
}

template<bool Indexed, class Visitor, class... Variants, size_t... indexes>
constexpr decltype(auto) build_array(std::index_sequence<indexes...>) {
  if constexpr (Indexed) {
    struct fun_application {
      static constexpr decltype(auto) apply(Visitor&& vis, Variants&&...) {
        return std::forward<Visitor>(vis)(in_place_index_t<indexes>()...);
      }
    };
    return &fun_application::apply;
  } else {
    struct fun_application {
      static constexpr decltype(auto) apply(Visitor&& vis, Variants&&... var) {
        return std::forward<Visitor>(vis)(get<indexes>(std::forward<Variants>(var))...);
      }
    };
    return &fun_application::apply;
  }
}

template <bool Indexed, class Visitor, class... Variants, size_t... Processed, size_t... Current, class... After>
constexpr auto build_array(std::index_sequence<Processed...>, std::index_sequence<Current...>, After... ls) {
  return std::array{ build_array<Indexed, Visitor, Variants...>(std::index_sequence<Processed..., Current>{}, ls...)... };
}

template<bool Indexed, class Visitor, class... Variants>
constexpr decltype(auto) build_array() {
  return build_array<Indexed, Visitor, Variants...>(std::index_sequence<>{},
                                           std::make_index_sequence<variant_size_v<std::decay_t<Variants>>>{}...);
}

template<class Visitor, class... Variants>
constexpr decltype(auto) visit(Visitor&& vis, Variants&&... variants) requires (is_variant_v<std::remove_cvref_t<Variants>> && ...){
  auto arr = build_array<false, Visitor, Variants...>();
  return at(arr, variants.index()...)(std::forward<Visitor>(vis), std::forward<Variants>(variants)...);
}

template<class Visitor, class... Variants>
constexpr decltype(auto) visit_indexed(Visitor&& vis, Variants&&... variants) {
  auto arr = build_array<true, Visitor, Variants...>();
  return at(arr, variants.index()...)(std::forward<Visitor>(vis), std::forward<Variants>(variants)...);
}