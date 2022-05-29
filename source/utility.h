#pragma once


template<class... T>
struct variant;

template<class T, class F, class...Args>
struct count_type {
  constexpr static int count = count_type<T, Args...>::count;
};

template<class T, class... Args>
struct count_type<T, T, Args...> {
  constexpr static int count = 1 + count_type<T, Args...>::count;
};

template<class T>
struct count_type<T, T> {
  constexpr static int count = 1;
};

template<class T, class F>
struct count_type<T, F> {
  constexpr static int count = 0;
};

struct bad_variant_access : public std::exception {
  using std::exception::exception;
};

template <typename T>
struct identity { using type = T; };

template <typename U, typename... Ts> struct choose_overload;

template <typename U> struct choose_overload<U> { void operator()() const; };

template<typename T, typename U>
concept no_narrowing = (!std::is_same_v<T, bool> || std::is_same_v<std::remove_cvref_t<U>, bool>)
    && requires(U&& t) {
  new T[1]{std::forward<U>(t)};
};


template <typename U, typename T, typename... Ts>
struct choose_overload<U, T, Ts...> : choose_overload<U, Ts...> {
  using choose_overload<U, Ts...>::operator();
  identity<T> operator()(T) requires no_narrowing<T,U>;
};

// void is a valid variant alternative, but "T operator()(T)" is ill-formed
// when T is void
template <typename... Ts>
struct choose_overload<void, Ts...> : choose_overload<Ts...> {
  using overload<Ts...>::operator();
  identity<void> operator()();
};

// Find the best match out of `Ts...` with `T` as the argument.
template <typename T, typename... Ts> // requires no_narrowing<typename std::result_of_t<overload<Ts...>(T)>::type, T>
using best_match = typename std::result_of_t<choose_overload<T, Ts...>(T)>::type;


template <std::size_t I, class T>
struct variant_alternative; /* undefined */

//template <std::size_t I, class... Types>
//struct variant_alternative<I, variant<Types...>>;

template <std::size_t I, class T>
struct variant_alternative<I, const T> {
  using type = const typename variant_alternative<I, T>::type;
};

template<std::size_t I, class T, class... Ts>
struct variant_alternative<I, variant<T, Ts...>> {
  using type = typename variant_alternative<I - 1, variant<Ts...>>::type;
};

template<class T, class... Ts>
struct variant_alternative<0, variant<T, Ts...>> {
  using type = T;
};

template <size_t I, class T>
using variant_alternative_t = typename variant_alternative<I, T>::type;

template<class T>
struct variant_size;


template <class T>
struct variant_size<const T> : std::integral_constant<std::size_t, variant_size<T>::value> {
  static constexpr std::size_t value = variant_size<T>::value;

  constexpr size_t operator()() const {
    return value;
  }

  constexpr operator size_t() const {
    return value;
  };
};

template<class... Args>
struct variant_size<variant<Args...>> : std::integral_constant<std::size_t, sizeof...(Args)> {
  static constexpr std::size_t value = sizeof...(Args);

  constexpr size_t operator()() const {
    return value;
  }

  constexpr operator size_t() const {
    return value;
  };

};

template <class T>
inline constexpr size_t variant_size_v = variant_size<T>::value;


template<class T, class F, class...Args>
struct find_type_index {
  constexpr static int count = 1 + find_type_index<T, Args...>::count;
};

template<class T, class... Args>
struct find_type_index<T, T, Args...> {
  constexpr static int count = 0;
};

template <class T>
struct is_variant {
  constexpr static bool value = false;
};

template<class... Args>
struct is_variant<variant<Args...>> {
  constexpr static bool value = true;
};

template<class T>
inline constexpr bool is_variant_v = is_variant<T>::value;


