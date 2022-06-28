#pragma once

#include "visit.h"
#include "equal.h"
#include <type_traits>
#include <utility>
#include <iostream>
#include <tuple>

template<class Type, class First, class...Rest>
constexpr size_t get_index_by_type(size_t ind = 0) {
  if constexpr (std::is_same_v<First, Type>) {
    return ind;
  } else {
    return get_index_by_type<Type, Rest...>(ind + 1);
  }
}

template<bool trivially_destructible, typename... Args>
struct variant_base {
  std::size_t ind = 0;
  variant_storage_t<0, (std::is_trivially_destructible_v<Args> && ...), Args...> storage;

  constexpr ~variant_base() {
    if (valueless_by_exception()) {
      return;
    }
    storage.destruct(index());
  }

  constexpr variant_base(empty_init_tag) : storage(empty_init_tag()) {

  }

  constexpr variant_base() : storage() {

  }

  template<size_t T, class... Types>
  constexpr variant_base(in_place_index_t<T> tag, Types&&... args) : storage(tag, std::forward<Types>(args)...) {}

  template<class T, class... Types>
  constexpr variant_base(in_place_type_t<T> tag, Types&&... args) : storage(tag, std::forward<Types>(args)...) {}

  constexpr bool valueless_by_exception() const {
    return index() == variant_npos;
  }

  constexpr std::size_t index() const noexcept {
    return this->ind;
  }
};

template<typename... Args>
struct variant_base<true, Args...> {
  std::size_t ind = 0;
  variant_storage_t<0, (std::is_trivially_destructible_v<Args> && ...) ,Args...> storage;

  constexpr variant_base() : storage() {

  }

  constexpr variant_base(empty_init_tag) : storage(empty_init_tag()) {}

  template<class T, class... Types>
  constexpr variant_base(in_place_type_t<T> tag, Types&&... args) : storage(tag, std::forward<Types>(args)...) {}

  template<size_t T, class... Types>
  constexpr variant_base(in_place_index_t<T> tag, Types&&... args) : storage(tag, std::forward<Types>(args)...) {}

  constexpr bool valueless_by_exception() const {
    return index() == variant_npos;
  }

  constexpr std::size_t index() const noexcept {
    return this->ind;
  }

  constexpr ~variant_base() = default;
};

template <class... Args>
struct variant : variant_base<(std::is_trivially_destructible_v<Args> && ...), Args...> {
public:

  template<size_t N>
  using NthType = std::tuple_element_t<N, std::tuple<Args...>>;

  using FirstType = std::tuple_element_t<0, std::tuple<Args...>>;

  constexpr variant() noexcept(std::is_nothrow_default_constructible_v<FirstType>) requires (std::is_default_constructible_v<FirstType>) : variant_base<(std::is_trivially_destructible_v<Args> && ...), Args...>() {
    this->ind = 0;
  }

  constexpr variant(const variant& rhs) requires(!(std::is_trivially_copy_constructible_v<Args> && ...) && (std::is_copy_constructible_v<Args> && ...)) : variant_base<(std::is_trivially_destructible_v<Args> && ...), Args...>(empty_init_tag()) {
    this->ind = rhs.index();
    if (!rhs.valueless_by_exception()) {
      visit_indexed([this, &rhs]<size_t T>(in_place_index_t<T>){
          this->storage.template get_storage<T>().init(rhs.storage.template get_storage<T>());
      }, rhs);
    }
  }

  constexpr variant(const variant& rhs) = default;

  // move constructor done

  constexpr variant(variant&& rhs) = delete;

  constexpr static bool trivially_move_constructor = (std::is_trivially_move_constructible_v<Args> && ...);

  constexpr variant(variant&& rhs) noexcept requires(trivially_move_constructor) = default;

  constexpr variant(variant&& rhs) noexcept((std::is_nothrow_move_constructible_v<Args> && ...))
      requires(!trivially_move_constructor && (std::is_move_constructible_v<Args> && ...)) : variant_base<(std::is_trivially_destructible_v<Args> && ...), Args...>(empty_init_tag()) {
    this->ind = rhs.index();
    if (!rhs.valueless_by_exception()) {
      visit_indexed([this, &rhs]<size_t T>(in_place_index_t<T>){
           this->storage.template get_storage<T>().init(std::move(rhs.storage.template get_storage<T>()));
      }, std::move(rhs));
      //storage.init(index(), std::move(rhs.storage));
    }
  }

  template <class T, class... Types>
  requires(count_type<T, Args...>::count == 1 &&
           std::is_constructible_v<T, Types...>) constexpr explicit variant(in_place_type_t<T> tag,
                                                                            Types&&... args)
      : variant_base<(std::is_trivially_destructible_v<Args> && ...), Args...>(tag, std::forward<Types>(args)...) {
    this->ind = get_index_by_type<T, Args...>();
  }

  template <std::size_t I, class... Types>
  requires((sizeof...(Args) > I) &&
           std::is_constructible_v<NthType<I>, Types...>) constexpr explicit variant(in_place_index_t<I> tag,
                                                                                     Types&&... args)
      : variant_base<(std::is_trivially_destructible_v<Args> && ...), Args...>(tag, std::forward<Types>(args)...) {
    this->ind = I;
  }

  // converting
  template< class T , class U = best_match<T, Args...>> requires ((sizeof...(Args) > 0) && std::is_constructible_v<U, T>)
      constexpr variant( T&& t ) noexcept(std::is_nothrow_constructible_v<U, T>) : variant_base<(std::is_trivially_destructible_v<Args> && ...), Args...>(in_place_type_t<U>(), std::forward<T>(t)) {
    this->ind = get_index_by_type<U, Args...>();
  }

  // Copy assignment done
  constexpr variant& operator=(const variant& rhs) = delete;

  static constexpr bool trivially_copy_assignment =
      ((std::is_trivially_copy_assignable_v<Args> && std::is_trivially_copy_constructible_v<Args> &&
        std::is_trivially_destructible_v<Args>)&&...);

  constexpr variant& operator=(const variant& rhs) requires(trivially_copy_assignment) = default;

  constexpr variant& operator=(const variant& rhs)
      requires(!trivially_copy_assignment &&
               ((std::is_copy_constructible_v<Args> && std::is_copy_assignable_v<Args>)&&...)) {
    if (this == &rhs) {
      return *this;
    }
    if (this->valueless_by_exception() && rhs.valueless_by_exception()) {
      return *this;
    }
    if (!this->valueless_by_exception() && rhs.valueless_by_exception()) {
      visit([]<class F>(F& val) { val.~F(); }, *this);
      this->ind = variant_npos;
      this->storage.reset();
      return *this;
    }
    visit_indexed(
        [this, &rhs]<size_t I, size_t J>(in_place_index_t<I>, in_place_index_t<J>) {
          if constexpr (I == J) {
            get<I>(*this) = get<I>(rhs);
          } else {
            this->emplace<J>(get<J>(rhs));
          }
        },
        *this, std::move(rhs));
    return *this;
  }

  // convert assignment
  template <class T, class U = best_match<T, Args...>>
  requires(!std::is_same_v<std::remove_cvref<T>, variant<Args...>> && std::is_assignable_v<U&, T> &&
           std::is_constructible_v<U, T>) constexpr variant&
  operator=(T&& t) noexcept(std::is_nothrow_assignable_v<U&, T>&& std::is_nothrow_constructible_v<U, T>) {
    if (this->index() == get_index_by_type<U, Args...>()) {
      visit_indexed([this, &t]<size_t I>(in_place_index_t<I>) {
        if constexpr (I == get_index_by_type<U, Args...>()) {
          get<I>(*this) = std::forward<T>(t);
        }
      }, *this);
      return *this;
    }
    if (std::is_nothrow_constructible_v<U, T> || !std::is_nothrow_move_constructible_v<U>) {
      try {
        this->emplace<U>(std::forward<T>(t));
      } catch (...) {
        this->ind = variant_npos;
      }
      return *this;
    } else {
      this->template emplace<U>(U(std::forward<T>(t)));
      return *this;
    }
  }

  // move assignment

  constexpr variant& operator=(variant&& rhs) = delete;

  static constexpr bool trivially_move_assignment = ((std::is_trivially_move_constructible_v<Args> && std::is_trivially_move_assignable_v<Args> && std::is_trivially_destructible_v<Args>) && ...);

  constexpr variant& operator=(variant&& rhs) noexcept requires(trivially_move_assignment) = default;

  constexpr variant& operator=(variant&& rhs) noexcept(((std::is_nothrow_move_constructible_v<Args> &&
                                                         std::is_nothrow_move_assignable_v<Args>)&&...))
      requires(!trivially_move_assignment &&
               ((std::is_move_assignable_v<Args> && std::is_move_constructible_v<Args>)&&...)) {
    if (this == &rhs) {
      return *this;
    }
    if (this->valueless_by_exception() && rhs.valueless_by_exception()) {
      return *this;
    }
    if (!this->valueless_by_exception() && rhs.valueless_by_exception()) {
      visit([]<class F>(F& val) { val.~F(); }, *this);
      this->storage.reset();
      this->ind = variant_npos;
      return *this;
    }
    visit_indexed(
        [this, &rhs]<size_t I, size_t J>(in_place_index_t<I>, in_place_index_t<J>) {
          if constexpr (I == J) {
            get<I>(*this) = get<J>(std::move(rhs));
          } else {
            this->emplace<J>(get<J>(std::move(rhs)));
          }
        },
        *this, std::move(rhs));
    return *this;
  }

  template<size_t N>
  constexpr decltype(auto) get_storage() {
    return this->storage.template get_storage<N>();
  }

  //emplace
  template<class T, class... CnstrArgs> requires (std::is_constructible_v<T, CnstrArgs...>)
  constexpr void emplace(CnstrArgs&& ... args) {
    static_assert(count_type<T, Args...>::count == 1);
    this->storage.destruct(this->index());
    this->storage.reset();
    this->ind = get_index_by_type<T, Args...>();
    visit_indexed([&args..., this]<size_t V>(in_place_index_t<V>){
      try {
        if constexpr (V == get_index_by_type<T, Args...>()) {
          new (std::addressof(this->storage.template get_storage<V>().value)) NthType<V>(std::forward<CnstrArgs>(args)...);
        }
      } catch (...) {
        this->ind = variant_npos;
        this->storage.reset();
        throw;
      }
    }, *this);
  }

  template <std::size_t I, class... CnstrArgs>
  requires(std::is_constructible_v<NthType<I>, CnstrArgs...>) constexpr void emplace(
      CnstrArgs&&... args) {
    static_assert(sizeof...(Args) > I);
    this->storage.destruct(this->index());
    this->storage.reset();
    this->ind = I;
    visit_indexed(
        [&args..., this]<size_t J>(in_place_index_t<J>) {
          if constexpr (I == J) { // нужно для обмана компилятор
            try {
              new (const_cast<typename std::remove_cv<NthType<J>>::type *>(std::addressof(this->storage.template get_storage<J>().value))) NthType<J>{std::forward<CnstrArgs>(args)...};
              //new () NthType<J>{std::forward<CnstrArgs>(args)...};
            } catch (...) {
              this->ind = variant_npos;
              throw;
            }
          }
        },
        *this);
  }

  // swap
  constexpr void swap( variant& rhs ) noexcept(((std::is_nothrow_move_constructible_v<Args> &&
                                               std::is_nothrow_swappable_v<Args>) && ...)) {
    if (this->valueless_by_exception() && rhs.valueless_by_exception()) {
      return;
    }
    if (this->index() == rhs.index()) {
      visit_indexed([this, &rhs]<size_t L, size_t R>(in_place_index_t<L>, in_place_index_t<R>) {
        if constexpr (L == R) {
          using std::swap;
          swap(get<L>(*this), get<R>(rhs));
        }
      }, *this, rhs);
    } else if (this->valueless_by_exception()) {
      this->storage.reset();
      visit_indexed([this, &rhs]<size_t T>(in_place_index_t<T>){
        this->storage.template get_storage<T>().init(std::move(rhs.storage.template get_storage<T>()));
      }, rhs);
      rhs.storage.reset();
    } else if (rhs.valueless_by_exception()) {
      rhs.storage.reset();
      visit_indexed([this, &rhs]<size_t T>(in_place_index_t<T>){
        rhs.storage.template get_storage<T>().init(std::move(this->storage.template get_storage<T>()));
      }, *this);
      this->storage.reset();
    } else {
      visit_indexed([this, &rhs]<size_t L, size_t R>(in_place_index_t<L>, in_place_index_t<R>){
        auto left_val = NthType<L>(std::move(this->storage.template get_storage<L>().value));
        auto right_val = NthType<R>(std::move(rhs.storage.template get_storage<R>().value));
        this->storage.reset();
        rhs.storage.reset();
        this->storage.template get_storage<R>().init(std::move(right_val));
        rhs.storage.template get_storage<L>().init(std::move(left_val));
      }, *this, rhs);
    }
    std::swap(this->ind, rhs.ind);
  }
};

template< class T, class... Types >
constexpr bool holds_alternative( const variant<Types...>& v ) noexcept {
  static_assert(count_type<T, Types...>::count == 1);
  return visit_indexed([]<size_t V>(in_place_index_t<V>) {
    return std::is_same_v<std::tuple_element_t<V, std::tuple<Types...>>, T>;
  }, v);
}