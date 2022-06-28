#pragma once
#include "utility.h"

struct empty_init_tag {};

constexpr std::size_t variant_npos = -1;

template <class T> struct in_place_type_t {
  explicit in_place_type_t() = default;
};
template <class T>
inline constexpr in_place_type_t<T> in_place_type{};

template <std::size_t I> struct in_place_index_t {
  explicit in_place_index_t() = default;
};
template <std::size_t I>
inline constexpr in_place_index_t<I> in_place_index{};

template<bool is_trivially_destructible>
struct variant_storage_base {
  ~variant_storage_base() {

  }
};


template <size_t index, bool is_trivially_destructible, class First, class... Rest>
union variant_storage_t {

  using type = First;

  First value;
  variant_storage_t<index + 1, is_trivially_destructible, Rest...> rest;

  constexpr variant_storage_t() : value(First()) {}

  constexpr variant_storage_t(empty_init_tag) : rest(empty_init_tag()) {}

  constexpr explicit variant_storage_t(First&& value) : value(std::move(value)) {}

  template<class T, class... Args>
  constexpr explicit variant_storage_t(in_place_type_t<T>, Args&&... args) requires (std::is_same_v<T, First>) : value(First(std::forward<Args>(args)...)) {}

  template<class T, class... Args>
  constexpr explicit variant_storage_t(in_place_type_t<T>, Args&&... args) requires (!std::is_same_v<T, First>) : rest(in_place_type_t<T>(), std::forward<Args>(args)...) {}

  template<size_t N, class... Args>
  constexpr variant_storage_t(in_place_index_t<N>, Args&&... args) requires (index == N) : value(First(std::forward<Args>(args)...)) {}

  template<size_t N, class... Args>
  constexpr variant_storage_t(in_place_index_t<N> tag, Args&&... args) requires (index != N) : rest(tag, std::forward<Args>(args)...) {}

  constexpr variant_storage_t(const variant_storage_t&) requires (std::is_trivially_copy_constructible_v<First> && (std::is_trivially_copy_constructible_v<Rest> && ...)) = default;

  constexpr void destruct(size_t ind) {
    if (ind == 0) {
      value.~First();
    } else {
      rest.destruct(ind - 1);
    }
  }
  constexpr void reset() {
    new (&rest) variant_storage_t<index + 1, is_trivially_destructible, Rest...>(empty_init_tag());
  }

  constexpr void init(const variant_storage_t& rhs) {
    new (std::addressof(value)) First(rhs.value);
  }

  constexpr void init(variant_storage_t&& rhs) {
    new (std::addressof(value)) First{std::move(rhs.value)};
  }

  constexpr void init(First&& rhs) {
    new (std::addressof(value)) First{std::move(rhs)};
  }


  template<size_t N>
  constexpr decltype(auto) get_storage() {
    if constexpr (N == 0) {
      return (*this);
    } else {
      return rest.template get_storage<N - 1>();
    }
  }

  template<size_t N>
  constexpr decltype(auto) get_storage() const {
    if constexpr (N == 0) {
      return (*this);
    } else {
      return rest.template get_storage<N - 1>();
    }
  }
  constexpr ~variant_storage_t() requires (!(std::is_trivially_destructible_v<First> && (std::is_trivially_destructible_v<Rest> && ...))) {}
};


template <size_t index, class First, class... Rest>
union variant_storage_t<index, true, First, Rest...> {

  using type = First;

  First value;
  variant_storage_t<index + 1, true, Rest...> rest;

  constexpr variant_storage_t() : value(First()) {}

  constexpr variant_storage_t(empty_init_tag) : rest(empty_init_tag()) {}

  constexpr explicit variant_storage_t(First&& value) : value(std::move(value)) {}

  template<class T, class... Args>
  constexpr explicit variant_storage_t(in_place_type_t<T>, Args&&... args) requires (std::is_same_v<T, First>) : value(First(std::forward<Args>(args)...)) {}

  template<class T, class... Args>
  constexpr explicit variant_storage_t(in_place_type_t<T>, Args&&... args) requires (!std::is_same_v<T, First>) : rest(in_place_type_t<T>(), std::forward<Args>(args)...) {}

  template<size_t N, class... Args>
  constexpr variant_storage_t(in_place_index_t<N>, Args&&... args) requires (index == N) : value(First(std::forward<Args>(args)...)) {}

  template<size_t N, class... Args>
  constexpr variant_storage_t(in_place_index_t<N> tag, Args&&... args) requires (index != N) : rest(tag, std::forward<Args>(args)...) {}

  constexpr variant_storage_t(const variant_storage_t&) requires (std::is_trivially_copy_constructible_v<First> && (std::is_trivially_copy_constructible_v<Rest> && ...)) = default;

  constexpr void destruct(size_t ind) {
    if (ind == 0) {
      value.~First();
    } else {
      rest.destruct(ind - 1);
    }
  }
  constexpr void reset() {
    new (&rest) variant_storage_t<index + 1, true, Rest...>(empty_init_tag());
  }

  constexpr void init(const variant_storage_t& rhs) {
    new (std::addressof(value)) First(rhs.value);
  }

  constexpr void init(variant_storage_t&& rhs) {
    new (std::addressof(value)) First{std::move(rhs.value)};
  }

  constexpr void init(First&& rhs) {
    new (std::addressof(value)) First{std::move(rhs)};
  }

  template<size_t N>
  constexpr decltype(auto) get_storage() {
    if constexpr (N == 0) {
      return (*this);
    } else {
      return rest.template get_storage<N - 1>();
    }
  }

  template<size_t N>
  constexpr decltype(auto) get_storage() const {
    if constexpr (N == 0) {
      return (*this);
    } else {
      return rest.template get_storage<N - 1>();
    }
  }
  constexpr ~variant_storage_t() = default;
};




template <size_t index, bool is_trivially_destructible, class First>
union variant_storage_t<index, is_trivially_destructible, First> {

  using type = First;

  First value;
  char dummy;

  constexpr variant_storage_t() : value(First()) {}

  constexpr variant_storage_t(empty_init_tag) : dummy('\0') {}

  constexpr variant_storage_t(const variant_storage_t&) requires (std::is_trivially_copy_constructible_v<First>) = default;

  template <class... Args>
  constexpr explicit variant_storage_t(in_place_type_t<First>, Args&&... args) : value(First(std::forward<Args>(args)...)) {}

  template<size_t N, class... Args>
  constexpr variant_storage_t(in_place_index_t<N>, Args&&... args) : value(First(std::forward<Args>(args)...)) {}

  template<size_t N>
  constexpr decltype(auto) get_storage() noexcept {
    return (*this);
  }

  template<size_t N>
  constexpr decltype(auto) get_storage() const noexcept {
    return (*this);
  }

  constexpr void init(const variant_storage_t& rhs) {
    new (std::addressof(value)) First{rhs.value};
  }

  constexpr void init(variant_storage_t&& rhs) {
    new (std::addressof(value)) First{std::move(rhs.value)};
  }

  constexpr void init(First&& rhs) {
    new (std::addressof(value)) First{std::move(rhs)};
  }

  constexpr void destruct(size_t) {
    value.~First();
  }

  constexpr void reset() {
    new (&dummy) char('\0');
  }

  constexpr ~variant_storage_t() {
  }
};

template <size_t index, class First>
union variant_storage_t<index, true, First> {

  using type = First;

  First value;
  char dummy;

  constexpr variant_storage_t() : value(First()) {}

  constexpr variant_storage_t(empty_init_tag) : dummy('\0') {}

  constexpr variant_storage_t(const variant_storage_t&) requires (std::is_trivially_copy_constructible_v<First>) = default;

  template <class... Args>
  constexpr explicit variant_storage_t(in_place_type_t<First>, Args&&... args) : value(First(std::forward<Args>(args)...)) {}

  template<size_t N, class... Args>
  constexpr variant_storage_t(in_place_index_t<N>, Args&&... args) : value(First(std::forward<Args>(args)...)) {}

  template<size_t N>
  constexpr decltype(auto) get_storage() noexcept {
    return (*this);
  }

  template<size_t N>
  constexpr decltype(auto) get_storage() const noexcept {
    return (*this);
  }

  constexpr void init(const variant_storage_t& rhs) {
    new (std::addressof(value)) First{rhs.value};
  }

  constexpr void init(variant_storage_t&& rhs) {
    new (std::addressof(value)) First{std::move(rhs.value)};
  }

  constexpr void init(First&& rhs) {
    new (std::addressof(value)) First{std::move(rhs)};
  }

  constexpr void destruct(size_t) {
    value.~First();
  }

  constexpr void reset() {
    new (&dummy) char('\0');
  }

  constexpr ~variant_storage_t() = default;
};