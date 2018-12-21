#include <type_traits>
#include <iostream>

//////////////////////////////////////////////////////////

template<typename T, T V>
struct contains
{};

//////////////////////////////////////////////////////////

template<typename T, T... V>
struct value_set : contains<T, V>...
{};

template<typename Stream, typename T, T... V>
Stream& operator << (Stream& s, const value_set<T, V...>&)
{
    s << "{ ";
    int dummy[] = { ((s << V << " "), 0)... };
    s << "}";
    return s;
}

//////////////////////////////////////////////////////////

template<typename Set, typename T, T V>
struct value_set_contains
{
    static std::true_type foo(contains<T, V>*);
    static std::false_type foo(...);

    static const bool value = decltype(foo(std::declval<Set*>()))::value;
};

template<typename Set, typename T, T V>
constexpr bool value_set_contains_v = value_set_contains<Set, T, V>::value;

//////////////////////////////////////////////////////////

template<int... V>
using int_set = value_set<int, V...>;

//////////////////////////////////////////////////////////

template<typename Set, int V>
constexpr bool int_set_contains_v = value_set_contains_v<Set, int, V>;

//////////////////////////////////////////////////////////

template<typename Set, typename T, T V>
struct insert_to_value_set
{};

template<typename T, T... Vs, T V>
struct insert_to_value_set<value_set<T, Vs...>, T, V>
{
    static value_set<T, Vs...> foo(contains<T, V>*);
    static value_set<T, Vs..., V> foo(...);

    using type = decltype(foo(std::declval<value_set<T, Vs...>*>()));
};

template<typename Set, typename T, T V>
using insert_to_value_set_t = typename insert_to_value_set<Set, T, V>::type;

//////////////////////////////////////////////////////////

template<typename Set, int V>
using insert_to_int_set_t = insert_to_value_set_t<Set, int, V>;

//////////////////////////////////////////////////////////

template<typename Set, typename T, T... V>
struct insert_range_to_value_set
{};

template<typename Set, typename T, T... V>
using insert_range_to_value_set_t = typename insert_range_to_value_set<Set, T, V...>::type;

template<typename Set, typename T, T First, T... Rest>
struct insert_range_to_value_set<Set, T, First, Rest...>
{
    using type = insert_range_to_value_set_t<
        insert_to_value_set_t<Set, T, First>,
        T,
        Rest...
    >;
};

template<typename Set, typename T>
struct insert_range_to_value_set<Set, T>
{
    using type = Set;
};

//////////////////////////////////////////////////////////

template<typename Set, int... V>
using insert_range_to_int_set_t = insert_range_to_value_set_t<Set, int, V...>;

//////////////////////////////////////////////////////////

template<typename T, typename... Sets>
struct add_value_sets
{};

template<typename T, typename... Sets>
using add_value_sets_t = typename add_value_sets<T, Sets...>::type;

template<typename T, typename Set, T... V, typename... Rest>
struct add_value_sets<T, Set, value_set<T, V...>, Rest...>
{
    using type = add_value_sets_t<
        T,
        insert_range_to_value_set_t<Set, T, V...>,
        Rest...
    >;
};

template<typename T, typename Set>
struct add_value_sets<T, Set>
{
    using type = Set;
};

template<typename T>
struct add_value_sets<T>
{
    using type = value_set<T>;
};

//////////////////////////////////////////////////////////

template<typename... Sets>
using add_int_sets_t = typename add_value_sets<int, Sets...>::type;

//////////////////////////////////////////////////////////

template<char C>
struct char_terminal
{
};

//////////////////////////////////////////////////////////

template<int V>
struct def
{};

template<int V>
using def_t = typename def<V>::type;

//////////////////////////////////////////////////////////

template<int... Seq>
struct is
{};

//////////////////////////////////////////////////////////

template<typename... Seq>
struct alt
{};

//////////////////////////////////////////////////////////

template<int V, typename Break, typename Enable>
struct nullable;

//////////////////////////////////////////////////////////

template<typename T, typename Break>
struct nullable_def
{};

template<char C, typename Break>
struct nullable_def<char_terminal<C>, Break>
{
    static const bool value = false;
};

template<typename... Seq, typename Break>
struct nullable_def<alt<Seq...>, Break>
{
    static const bool value = std::disjunction_v<nullable_def<Seq, Break>...>;
};

template<int... Seq, typename Break>
struct nullable_def<is<Seq...>, Break>
{
    static const bool value = std::conjunction_v<nullable<Seq, Break>...>;
};

template<typename Break>
struct nullable_def<is<>, Break>
{
    static const bool value = true;
};

//////////////////////////////////////////////////////////

template<int V, typename Break = int_set<>, typename Enable = void>
struct nullable
{
    static const bool value = false;
};

template<int V, typename Break>
struct nullable<V, Break, std::enable_if_t<!int_set_contains_v<Break, V>>>
{
    static const bool value = nullable_def<def_t<V>, insert_to_int_set_t<Break, V>>::value;
};

template<int V>
constexpr bool nullable_v = nullable<V>::value;

//////////////////////////////////////////////////////////

template<int V, typename Break = int_set<>, typename Enable = void>
struct first_set
{
    using type = int_set<>;
};

//////////////////////////////////////////////////////////

template<typename Seq, typename Break, typename Enable = void>
struct first_set_for_sequence
{};

template<int First, int... Rest, typename Break>
struct first_set_for_sequence<is<First, Rest...>, Break, std::enable_if_t<nullable_v<First>>>
{
    using type = add_int_sets_t<
        typename first_set<First, Break>::type,
        typename first_set_for_sequence<is<Rest...>, Break>::type
    >;
};

template<int First, int... Rest, typename Break>
struct first_set_for_sequence<is<First, Rest...>, Break, std::enable_if_t<!nullable_v<First>>>
{
    using type = typename first_set<First, Break>::type;
};

template<typename Break, typename Enable>
struct first_set_for_sequence<is<>, Break, Enable>
{
    using type = int_set<>;
};

//////////////////////////////////////////////////////////

template<typename T, int V, typename Break>
struct first_set_for_def
{};

template<char C, int V, typename Break>
struct first_set_for_def<char_terminal<C>, V, Break>
{
    using type = int_set<V>;
};

template<typename... Seq, int V, typename Break>
struct first_set_for_def<alt<Seq...>, V, Break>
{
    using type = add_int_sets_t<
        typename first_set_for_sequence<Seq, Break>::type...
    >;
};

template<int... Seq, int V, typename Break>
struct first_set_for_def<is<Seq...>, V, Break>
{
    using type = typename first_set_for_sequence<is<Seq...>, Break>::type;
};

template<int V, typename Break>
struct first_set_for_def<is<>, V, Break>
{
    using type = int_set<>;
};

//////////////////////////////////////////////////////////

template<int V, typename Break>
struct first_set<V, Break, std::enable_if_t<!int_set_contains_v<Break, V>>>
{
    using type = typename first_set_for_def<def_t<V>, V, insert_to_int_set_t<Break, V>>::type;
};

template<int V>
using first_set_t = typename first_set<V>::type;

//////////////////////////////////////////////////////////

template<int S>
struct parser
{};

//////////////////////////////////////////////////////////

enum symbols 
{
    ex, opt,
    plus, minus, one 
};

template<>
struct def<plus>
{
    using type = char_terminal<'+'>;
};

template<>
struct def<minus>
{
    using type = char_terminal<'-'>;
};

template<>
struct def<one>
{
    using type = char_terminal<'1'>;
};

template<>
struct def<opt>
{
    using type = alt<is<>, is<minus>>;
};

template<>
struct def<ex>
{
    using type = alt<is<opt, plus>>;
};

int main()
{
    using p = parser<ex>;

    std::cout << first_set_t<ex>{} << std::endl;
    
    return 0;
}