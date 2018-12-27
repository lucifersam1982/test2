#include <type_traits>
#include <utility>
#include <string_view>

#include <iostream>

//////////////////////////////////////////////////////////

template<int...V>
using int_sequence = std::integer_sequence<int, V...>;

template<int Size>
using make_int_sequence = std::make_integer_sequence<int, Size>;

//////////////////////////////////////////////////////////

template<typename Seq, int V>
struct add_to_int_sequence
{};

template<int... Seq, int V>
struct add_to_int_sequence<int_sequence<Seq...>, V>
{
    using type = int_sequence<Seq..., V>;
};

template<typename Seq, int V>
using add_to_int_sequence_t = typename add_to_int_sequence<Seq, V>::type;

//////////////////////////////////////////////////////////

template<int I1, int I2>
struct int_pair
{};

//////////////////////////////////////////////////////////

template<int V>
using int_constant = std::integral_constant<int, V>;

//////////////////////////////////////////////////////////

template<typename... Types>
struct type_sequence
{
    static const int size = sizeof...(Types);
};

//////////////////////////////////////////////////////////

template<typename Seq, typename T>
struct add_to_type_sequence
{};

template<typename... Seq, typename T>
struct add_to_type_sequence<type_sequence<Seq...>, T>
{
    using type = type_sequence<Seq..., T>;
};

template<typename Seq, typename T>
using add_to_type_sequence_t = typename add_to_type_sequence<Seq, T>::type;

//////////////////////////////////////////////////////////

template<typename T, T V>
struct contains
{};

//////////////////////////////////////////////////////////

template<typename T, T... V>
struct value_set : contains<T, V>...
{};

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

//////////////////////////////////////////////////////////

template<typename... Sets>
using add_int_sets_t = typename add_value_sets<int, Sets...>::type;

//////////////////////////////////////////////////////////

template<typename T, int I, T V>
struct contains_at : contains<T, V>
{};

//////////////////////////////////////////////////////////

template<typename T, typename Seq, T... V>
struct ordered_value_set_impl
{};

template<typename T, int... I, T... V>
struct ordered_value_set_impl<T, int_sequence<I...>, V...> : contains_at<T, I, V>...
{};

template<typename T, T... V>
struct ordered_value_set : ordered_value_set_impl<T, make_int_sequence<sizeof...(V)>, V...>
{};

//////////////////////////////////////////////////////////

template<typename Set, int At>
struct get_at_idx
{
    template<typename T, T V>
    static int_constant<V> foo(contains_at<T, At, V>*);

    static const auto value = decltype(foo(std::declval<Set*>()))::value;
};

//////////////////////////////////////////////////////////

template<typename T, T... Vs, T V>
struct insert_to_value_set<ordered_value_set<T, Vs...>, T, V>
{
    static ordered_value_set<T, Vs...> foo(contains<T, V>*);
    static ordered_value_set<T, Vs..., V> foo(...);

    using type = decltype(foo(std::declval<ordered_value_set<T, Vs...>*>()));
};

//////////////////////////////////////////////////////////

template<typename T, typename Set, T... V, typename... Rest>
struct add_value_sets<T, Set, ordered_value_set<T, V...>, Rest...>
{
    using type = add_value_sets_t<
        T,
        insert_range_to_value_set_t<Set, T, V...>,
        Rest...
    >;
};

//////////////////////////////////////////////////////////

template<int...V>
using ordered_int_set = ordered_value_set<int, V...>;

//////////////////////////////////////////////////////////

struct terminal
{};

//////////////////////////////////////////////////////////

template<char C>
struct char_terminal : terminal
{
    static const char* debug_name()
    {
        static char n[2] = { C, 0 };
        return n;
    }
};

//////////////////////////////////////////////////////////

struct end_of_input : terminal
{};

//////////////////////////////////////////////////////////

template<int V>
struct def
{};

template<int V>
using def_t = typename def<V>::type;

//////////////////////////////////////////////////////////

template<typename T>
constexpr bool is_terminal = std::is_base_of_v<terminal, T>;

//////////////////////////////////////////////////////////

template<typename T>
constexpr bool is_nonterminal = !is_terminal<T>;

//////////////////////////////////////////////////////////

template<int... Seq>
struct is
{};

//////////////////////////////////////////////////////////

template<typename... Seq>
struct alt
{};

//////////////////////////////////////////////////////////

template<int V, typename Break, typename Enable = void>
struct nullable;

//////////////////////////////////////////////////////////

template<typename T, typename Break, typename Enable = void>
struct nullable_def
{};

template<typename T, typename Break>
struct nullable_def<T, Break, std::enable_if_t<is_terminal<T>>>
{
    static const bool value = false;
};

template<typename... Seq, typename Break, typename Enable>
struct nullable_def<alt<Seq...>, Break, Enable>
{
    static const bool value = std::disjunction_v<nullable_def<Seq, Break>...>;
};

template<int... Seq, typename Break, typename Enable>
struct nullable_def<is<Seq...>, Break, Enable>
{
    static const bool value = std::conjunction_v<nullable<Seq, Break>...>;
};

template<typename Break, typename Enable>
struct nullable_def<is<>, Break, Enable>
{
    static const bool value = true;
};

//////////////////////////////////////////////////////////

template<typename Seq>
constexpr bool nullable_def_v = nullable_def<Seq, int_set<>>::value;

//////////////////////////////////////////////////////////

template<int V, typename Break = int_set<>, typename Enable>
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

template<typename Seq>
using first_set_for_sequence_t = typename first_set_for_sequence<Seq, int_set<>>::type;

//////////////////////////////////////////////////////////

template<typename T, int V, typename Break, typename Enable = void>
struct first_set_for_def
{};

template<typename T, int V, typename Break>
struct first_set_for_def<T, V, Break, std::enable_if_t<is_terminal<T>>>
{
    using type = int_set<V>;
};

template<typename... Seq, int V, typename Break, typename Enable>
struct first_set_for_def<alt<Seq...>, V, Break, Enable>
{
    using type = add_int_sets_t<
        typename first_set_for_sequence<Seq, Break>::type...
    >;
};

template<int... Seq, int V, typename Break, typename Enable>
struct first_set_for_def<is<Seq...>, V, Break, Enable>
{
    using type = typename first_set_for_sequence<is<Seq...>, Break>::type;
};

template<int V, typename Break, typename Enable>
struct first_set_for_def<is<>, V, Break, Enable>
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

template<int V, typename Break = int_set<>, typename Enable = void>
struct terminal_set
{
    using type = ordered_int_set<>;
};

//////////////////////////////////////////////////////////

template<typename Seq, typename Break>
struct terminal_set_for_sequence
{};

template<int... Seq, typename Break>
struct terminal_set_for_sequence<is<Seq...>, Break>
{
    using type = add_int_sets_t<
        typename terminal_set<Seq, Break>::type...
    >;
};

template<typename Break>
struct terminal_set_for_sequence<is<>, Break>
{
    using type = ordered_int_set<>;
};

//////////////////////////////////////////////////////////

template<typename T, int V, typename Break>
struct terminal_set_for_def
{};

template<char C, int V, typename Break>
struct terminal_set_for_def<char_terminal<C>, V, Break>
{
    using type = ordered_int_set<V>;
};

template<typename... Seq, int V, typename Break>
struct terminal_set_for_def<alt<Seq...>, V, Break>
{
    using type = add_int_sets_t<
        typename terminal_set_for_sequence<Seq, Break>::type...
    >;
};

template<int... Seq, int V, typename Break>
struct terminal_set_for_def<is<Seq...>, V, Break>
{
    using type = typename terminal_set_for_sequence<is<Seq...>, Break>::type;
};

template<int V, typename Break>
struct terminal_set_for_def<is<>, V, Break>
{
    using type = ordered_int_set<>;
};

//////////////////////////////////////////////////////////

template<int V, typename Break>
struct terminal_set<V, Break, std::enable_if_t<!int_set_contains_v<Break, V>>>
{
    using type = typename terminal_set_for_def<def_t<V>, V, insert_to_int_set_t<Break, V>>::type;
};

template<int V>
using terminal_set_t = typename terminal_set<V>::type;

//////////////////////////////////////////////////////////

template<int V, typename Break = int_set<>, typename Enable = void>
struct non_terminal_set
{
    using type = ordered_int_set<>;
};

//////////////////////////////////////////////////////////

template<typename Seq, typename Break>
struct non_terminal_set_for_sequence
{};

template<int... Seq, typename Break>
struct non_terminal_set_for_sequence<is<Seq...>, Break>
{
    using type = add_int_sets_t<
        typename non_terminal_set<Seq, Break>::type...
    >;
};

template<typename Break>
struct non_terminal_set_for_sequence<is<>, Break>
{
    using type = ordered_int_set<>;
};

//////////////////////////////////////////////////////////

template<typename T, int V, typename Break>
struct non_terminal_set_for_def
{};

template<char C, int V, typename Break>
struct non_terminal_set_for_def<char_terminal<C>, V, Break>
{
    using type = ordered_int_set<>;
};

template<typename... Seq, int V, typename Break>
struct non_terminal_set_for_def<alt<Seq...>, V, Break>
{
    using type = add_int_sets_t<
        ordered_int_set<V>,
        typename non_terminal_set_for_sequence<Seq, Break>::type...
    >;
};

template<int... Seq, int V, typename Break>
struct non_terminal_set_for_def<is<Seq...>, V, Break>
{
    using type = add_int_sets_t<
        ordered_int_set<V>,
        typename non_terminal_set_for_sequence<is<Seq...>, Break>::type
    >;
};

template<int V, typename Break>
struct non_terminal_set_for_def<is<>, V, Break>
{
    using type = ordered_int_set<V>;
};

//////////////////////////////////////////////////////////

template<int V, typename Break>
struct non_terminal_set<V, Break, std::enable_if_t<!int_set_contains_v<Break, V>>>
{
    using type = typename non_terminal_set_for_def<def_t<V>, V, insert_to_int_set_t<Break, V>>::type;
};

template<int V>
using nonterminal_set_t = typename non_terminal_set<V>::type;

//////////////////////////////////////////////////////////

template<int L, typename Alpha, int B, typename Beta, typename TSet>
struct situation
{
    static const int b = B;
};

//////////////////////////////////////////////////////////

template<int I, typename S>
struct contains_situation
{};

//////////////////////////////////////////////////////////

template<typename Seq, typename... S>
struct situation_set_impl
{};

template<int... I, typename... S>
struct situation_set_impl<int_sequence<I...>, S...> : contains_situation<I, S>...
{};

template<typename... S>
struct situation_set : situation_set_impl<make_int_sequence<sizeof...(S)>, S...>
{};

//////////////////////////////////////////////////////////

template<typename Set, int I>
struct get_situation_at_idx
{
    template<typename S>
    static S foo(contains_situation<I, S>*);
    
    using type = decltype(foo(std::declval<Set*>()));
};

template<typename Set, int I>
using get_situation_at_idx_t = typename get_situation_at_idx<Set, I>::type;

//////////////////////////////////////////////////////////

template<typename S, typename NewTSet>
struct update_situation
{};

template<int L, typename Alpha, int B, typename Beta, typename TSet, typename NewTSet>
struct update_situation<situation<L, Alpha, B, Beta, TSet>, NewTSet>
{
    using type = situation<L, Alpha, B, Beta, add_int_sets_t<TSet, NewTSet>>;
};

template<typename S, typename TSet>
using update_situation_t = typename update_situation<S, TSet>::type;

//////////////////////////////////////////////////////////

template<typename Set, typename Seq, int I, typename TSet>
struct update_situation_in_set_impl
{};

template<typename... S, int... V, int I, typename TSet>
struct update_situation_in_set_impl<situation_set<S...>, int_sequence<V...>, I, TSet>
{
    using type = situation_set<std::conditional_t<I == V, update_situation_t<S, TSet>, S>...>;
};

template<typename Set, int I, typename TSet>
struct update_situation_in_set
{};

template<typename... S, int I, typename TSet>
struct update_situation_in_set<situation_set<S...>, I, TSet>
{
    using type = typename update_situation_in_set_impl<
        situation_set<S...>,
        make_int_sequence<sizeof...(S)>,
        I,
        TSet
    >::type;
};

template<typename Set, int I, typename TSet>
using update_situation_in_set_t = typename update_situation_in_set<Set, I, TSet>::type;

//////////////////////////////////////////////////////////

template<typename Set, typename S>
struct insert_situation_to_set
{};

template<typename... S, int L, typename Alpha, int B, typename Beta, typename TSet>
struct insert_situation_to_set<situation_set<S...>, situation<L, Alpha, B, Beta, TSet>>
{
    template<int I, typename TSet2>
    static update_situation_in_set_t<situation_set<S...>, I, TSet> foo(contains_situation<I, situation<L, Alpha, B, Beta, TSet2>>*);

    static situation_set<S..., situation<L, Alpha, B, Beta, TSet>> foo(...);

    using type = decltype(foo(std::declval<situation_set<S...>*>()));
};

template<typename Set, typename S>
using insert_situation_to_set_t = typename insert_situation_to_set<Set, S>::type;

//////////////////////////////////////////////////////////

template<typename Set, typename Situations>
struct add_situations
{};

template<typename Set, typename Situations>
using add_situations_t = typename add_situations<Set, Situations>::type;

template<typename Set, typename First, typename... Rest>
struct add_situations<Set, type_sequence<First, Rest...>>
{
    using type = add_situations_t<
        insert_situation_to_set_t<Set, First>,
        type_sequence<Rest...>
    >;
};

template<typename Set>
struct add_situations<Set, type_sequence<>>
{
    using type = Set;
};

//////////////////////////////////////////////////////////

constexpr int nothing = -1;

//////////////////////////////////////////////////////////

template<int V, typename Seq, typename TSet>
struct seq_closure
{};

template<int V, int First, int... Rest, typename TSet>
struct seq_closure<V, is<First, Rest...>, TSet>
{
    using type = situation<V, int_sequence<>, First, int_sequence<Rest...>, TSet>;
};

template<int V, typename TSet>
struct seq_closure<V, is<>, TSet>
{
    using type = situation<V, int_sequence<>, nothing, int_sequence<>, TSet>;
};

//////////////////////////////////////////////////////////

template<int V, typename T, typename TSet>
struct def_closure
{};

template<int V, typename... Seq, typename TSet>
struct def_closure<V, alt<Seq...>, TSet>
{
    using type = type_sequence<
        typename seq_closure<V, Seq, TSet>::type...
    >;
};

template<int V, int... Seq, typename TSet>
struct def_closure<V, is<Seq...>, TSet>
{
    using type = type_sequence<
        typename seq_closure<V, is<Seq...>, TSet>::type
    >;
};

//////////////////////////////////////////////////////////

template<int V, typename TSet>
struct closure
{
    using type = typename def_closure<V, def_t<V>, TSet>::type;
};

template<int V, typename TSet>
using closure_t = typename closure<V, TSet>::type;

//////////////////////////////////////////////////////////

template<typename Situation, typename Enable = void>
struct situation_closure
{
    using type = type_sequence<>;
};

template<int L, typename Alpha, int B, int... Beta, typename TSet>
struct situation_closure
<
    situation<L, Alpha, B, int_sequence<Beta...>, TSet>,
    std::enable_if_t<is_nonterminal<def_t<B>>>
>
{
    using beta_first = first_set_for_sequence_t<is<Beta...>>;
    using type = closure_t<
        B, 
        std::conditional_t<
            nullable_def_v<is<Beta...>>,
            add_int_sets_t<
                beta_first, 
                TSet
            >,
            beta_first
        >
    >;
};

template<int L, typename Alpha, typename TSet>
struct situation_closure<situation<L, Alpha, nothing, int_sequence<>, TSet>>
{
    using type = type_sequence<>;
};

template<typename Situation>
using situation_closure_t = typename situation_closure<Situation>::type;

//////////////////////////////////////////////////////////

template<int I, typename Set, typename Enable = void>
struct recursive_closure
{
    using new_situations = add_situations_t<
        Set,
        situation_closure_t<get_situation_at_idx_t<Set, I>>
    >;

    using type = typename recursive_closure<
        I + 1,
        new_situations
    >::type;
};

template<int I, typename... Situations>
struct recursive_closure<I, situation_set<Situations...>, std::enable_if_t<I == sizeof...(Situations)>>
{
    using type = situation_set<Situations...>;
};

template<typename Situations>
using recursive_closure_t = typename recursive_closure<0, Situations>::type;

//////////////////////////////////////////////////////////

template<int V, typename Seq>
struct situation_map_element
{};

template<int V, int First, int... Rest>
struct situation_map_element<V, int_sequence<First, Rest...>>
{
    static const int front = First;
};

//////////////////////////////////////////////////////////

template<typename... Elements>
struct situation_map
{};

template<int... V, typename... Seq>
struct situation_map<situation_map_element<V, Seq>...> : situation_map_element<V, Seq>...
{};

//////////////////////////////////////////////////////////


template<typename Map, typename Sequence, int V, typename NewSeq>
struct update_situation_map_element_impl
{};

template<typename... Elements, int... I, int V, typename NewSeq>
struct update_situation_map_element_impl<situation_map<Elements...>, int_sequence<I...>, V, NewSeq>
{
    using type = situation_map<
        std::conditional_t<
            I == V,
            situation_map_element<V, NewSeq>,
            Elements
        >...
    >;
};

template<typename Map, int V, typename NewSeq>
struct update_situation_map_element
{};

template<typename... Elements, int V, typename NewSeq>
struct update_situation_map_element<situation_map<Elements...>, V, NewSeq>
{
    using type = typename update_situation_map_element_impl<
        situation_map<Elements...>,
        make_int_sequence<sizeof...(Elements)>, 
        V, 
        NewSeq
    >::type;
};

template<typename Map, int V, typename NewSeq>
using update_situation_map_element_t = typename update_situation_map_element<Map, V, NewSeq>::type;

//////////////////////////////////////////////////////////

template<typename Map, int V, int Idx>
struct add_to_situation_map
{};

template<typename... Elements, int V, int Idx>
struct add_to_situation_map<situation_map<Elements...>, V, Idx>
{
    template<typename Seq>
    static update_situation_map_element_t<
        situation_map<Elements...>, 
        V, 
        add_to_int_sequence_t<Seq, Idx>
    > foo(situation_map_element<V, Seq>*);
    
    static situation_map<Elements..., situation_map_element<V, int_sequence<Idx>>> foo(...);

    using type = decltype(foo(std::declval<situation_map<Elements...>*>()));
};

template<typename Map, int V, int Idx>
using add_to_situation_map_t = typename add_to_situation_map<Map, V, Idx>::type;

//////////////////////////////////////////////////////////

template<typename Map, typename... Elements>
struct add_range_to_situation_map
{};

template<typename Map, typename... Elements>
using add_range_to_situation_map_t = typename add_range_to_situation_map<Map, Elements...>::type;

template<typename Map, int V, int Idx, typename... Rest>
struct add_range_to_situation_map<Map, int_pair<V, Idx>, Rest...>
{
    using type = add_range_to_situation_map_t<
        add_to_situation_map_t<Map, V, Idx>,
        Rest...
    >;
};

template<typename Map, int Idx, typename... Rest>
struct add_range_to_situation_map<Map, int_pair<nothing, Idx>, Rest...>
{
    using type = typename add_range_to_situation_map<Map, Rest...>::type;
};

template<typename Map>
struct add_range_to_situation_map<Map>
{
    using type = Map;
};

//////////////////////////////////////////////////////////

template<typename Situations, typename Seq>
struct make_situation_map
{};

template<typename... Situations, int... I>
struct make_situation_map<situation_set<Situations...>, int_sequence<I...>>
{
    using type = add_range_to_situation_map_t<
        situation_map<>,
        int_pair<Situations::b, I>...
    >;
};

template<typename Situations, typename Seq>
using make_situation_map_t = typename make_situation_map<Situations, Seq>::type;

//////////////////////////////////////////////////////////

template<int Nr, typename Situations>
struct state
{};

template<int Nr, typename... Situations>
struct state<Nr, situation_set<Situations...>>
{
    static const int nr = Nr;

    using situations = situation_set<Situations...>;
    
    using map = make_situation_map_t<
        situation_set<Situations...>,
        make_int_sequence<sizeof...(Situations)>
    >;
};

//////////////////////////////////////////////////////////

template<int Nr, typename Situations>
struct make_state
{};

template<int Nr, typename... S>
struct make_state<Nr, type_sequence<S...>>
{
    using type = state<Nr, recursive_closure_t<situation_set<S...>>>;
};

template<int Nr, typename Situations>
using make_state_t = typename make_state<Nr, Situations>::type;

//////////////////////////////////////////////////////////

constexpr int no_state = -1;

//////////////////////////////////////////////////////////

template<typename StateSet, typename Situation>
struct get_state_nr_with_situation
{
    template<int I>
    static int_constant<I> foo(contains_situation<I, Situation>*);
    static int_constant<no_state> foo(...);

    static const int value = decltype(foo(std::declval<StateSet*>()))::value;
};

template<typename StateSet, typename Situation>
constexpr int get_state_nr_with_situation_v = get_state_nr_with_situation<StateSet, Situation>::value;

//////////////////////////////////////////////////////////

template<int Nr, typename Situations>
struct contains_situations
{};

template<int Nr, typename... Situations>
struct contains_situations<Nr, situation_set<Situations...>> : contains_situation<Nr, Situations>...
{};

//////////////////////////////////////////////////////////

template<int I, typename State>
struct contains_state : contains_situations<State::nr, typename State::situations>
{};

//////////////////////////////////////////////////////////

template<typename Seq, typename... States>
struct states_set_impl
{};

template<int... I, typename... States>
struct states_set_impl<int_sequence<I...>, States...> : contains_state<I, States>...
{};

template<typename... States>
struct states_set : states_set_impl<make_int_sequence<sizeof...(States)>, States...>
{
    static const int size = sizeof...(States);
};

//////////////////////////////////////////////////////////

template<typename Set, int I>
struct get_state_at_idx
{
    template<typename S>
    static S foo(contains_state<I, S>*);

    using type = decltype(foo(std::declval<Set*>()));
};

template<typename Set, int I>
using get_state_at_idx_t = typename get_state_at_idx<Set, I>::type;

//////////////////////////////////////////////////////////

template<int V, typename Situations>
struct new_state_transition
{};

//////////////////////////////////////////////////////////

template<int V, int Nr>
struct existing_state_transition
{};

//////////////////////////////////////////////////////////

template<typename Set, typename Transitions>
struct add_states
{};

template<typename Set, typename Transitions>
using add_states_t = typename add_states<Set, Transitions>::type;

template<typename... States, int V, typename Situations, typename... Rest>
struct add_states<states_set<States...>, type_sequence<new_state_transition<V, Situations>, Rest...>>
{
    using type = add_states_t<
        states_set<States..., make_state_t<sizeof...(States), Situations>>,
        type_sequence<Rest...>
    >;
};

template<typename... States, int V, int Nr, typename... Rest>
struct add_states<states_set<States...>, type_sequence<existing_state_transition<V, Nr>, Rest...>>
{
    using type = add_states_t<
        states_set<States...>,
        type_sequence<Rest...>
    >;
};

template<typename Set>
struct add_states<Set, type_sequence<>>
{
    using type = Set;
};

//////////////////////////////////////////////////////////

template<typename Situation>
struct move_situation
{};

template<int L, int... Alpha, int B, int First, int... Rest, typename TSet>
struct move_situation<situation<L, int_sequence<Alpha...>, B, int_sequence<First, Rest...>, TSet>>
{
    using type = situation<L, int_sequence<Alpha..., B>, First, int_sequence<Rest...>, TSet>;
};

template<int L, int... Alpha, int B, typename TSet>
struct move_situation<situation<L, int_sequence<Alpha...>, B, int_sequence<>, TSet>>
{
    using type = situation<L, int_sequence<Alpha..., B>, nothing, int_sequence<>, TSet>;
};

template<typename Situation>
using move_situation_t = typename move_situation<Situation>::type;

//////////////////////////////////////////////////////////

template<typename Element, int Nr, typename Situations>
struct make_transition_impl
{};

template<int V, int... I, typename Situations>
struct make_transition_impl<situation_map_element<V, int_sequence<I...>>, no_state, Situations>
{
    using type = new_state_transition<
        V, 
        type_sequence<
            move_situation_t<get_situation_at_idx_t<Situations, I>>...
        >
    >;
};

template<int V, typename Seq, int Nr, typename Situations>
struct make_transition_impl<situation_map_element<V, Seq>, Nr, Situations>
{
    using type = existing_state_transition<V, Nr>;
};

template<typename Element, typename Situations, typename StatesSet>
struct make_transition
{
    static const int state_nr = get_state_nr_with_situation_v<
        StatesSet,
        move_situation_t<get_situation_at_idx_t<Situations, Element::front>>
    >;

    using type = typename make_transition_impl<Element, state_nr, Situations>::type;
};

template<typename Element, typename Situations, typename StatesSet>
using make_transition_t = typename make_transition<Element, Situations, StatesSet>::type;

//////////////////////////////////////////////////////////

template<typename Map, typename Situations, typename StatesSet>
struct make_transitions
{};

template<typename... Elements, typename Situations, typename StatesSet>
struct make_transitions<situation_map<Elements...>, Situations, StatesSet>
{
    using type = type_sequence<make_transition_t<Elements, Situations, StatesSet>...>;
};

template<typename State, typename Set, typename StatesSet>
using make_transitions_t = typename make_transitions<State, Set, StatesSet>::type;

//////////////////////////////////////////////////////////

template<int I, typename Set, typename Enable = void>
struct recursive_transitions
{
    using current_state = get_state_at_idx_t<Set, I>;
    using current_transitions = make_transitions_t<
        typename current_state::map, 
        typename current_state::situations, 
        Set
    >;
    
    using new_set = add_states_t<Set, current_transitions>;
    using type = typename recursive_transitions<I + 1, new_set>::type;
};

template<int I, typename... States>
struct recursive_transitions<I, states_set<States...>, std::enable_if_t<I == sizeof...(States)>>
{
    using type = states_set<States...>;
};

template<typename Set>
using recursive_transitions_t = typename recursive_transitions<0, Set>::type;

//////////////////////////////////////////////////////////

using match_f = int(*)(const std::string_view&, int);

//////////////////////////////////////////////////////////

template<typename T>
struct matcher
{};

template<char C>
struct matcher<char_terminal<C>>
{
    bool operator()(const std::string_view& sv) const
    {
        return sv.size() == 1 && sv.front() == C;
    }
};

template<>
struct matcher<end_of_input>
{
    bool operator()(const std::string_view& sv) const
    {
        return sv.empty();
    }
};

//////////////////////////////////////////////////////////

template<int S>
class parser;

//////////////////////////////////////////////////////////

constexpr int no_match = -1;

//////////////////////////////////////////////////////////

template<typename First, typename... Rest>
inline int match(const std::string_view& sv, int x)
{
    const matcher<First> m;
    if (m(sv))
        return x;
    if constexpr (sizeof...(Rest) > 0)
        return match<Rest...>(sv, x + 1);
    else
        return no_match;
}

//////////////////////////////////////////////////////////

template<typename Set>
struct match_function_for_terminal_set
{};

template<int... V>
struct match_function_for_terminal_set<ordered_int_set<V...>>
{
    static inline match_f f = match<def_t<V>..., end_of_input>;
};

//////////////////////////////////////////////////////////

template <typename, typename = void>
struct has_debug_name : std::false_type
{};

template <typename T>
struct has_debug_name<T, std::void_t<decltype(T::debug_name())>> : std::true_type
{};

//////////////////////////////////////////////////////////

template<int V>
inline const char* def_debug_name_impl(def<V>, std::true_type)
{
    return def<V>::debug_name();
}

template<int V>
inline const char* def_debug_name_impl(def<V>, std::false_type)
{
    return def_t<V>::debug_name();
}

template<int V>
inline const char* def_debug_name(def<V>)
{
    return def_debug_name_impl(def<V>{}, has_debug_name<def<V>>{});
}

//////////////////////////////////////////////////////////

constexpr int end_of_input_v = -1;

template<>
struct def<end_of_input_v>
{
    using type = end_of_input;

    static const char* debug_name() { return "$"; }
};

//////////////////////////////////////////////////////////

template<typename TSet>
class debug_names
{};

template<int... V>
class debug_names<int_set<V...>>
{
public:
    auto operator[](int i) const
    {
        return _names[i];
    }

private:
    const char* _names[sizeof...(V) + 1] = { def_debug_name(def<V>{})... };
};

//////////////////////////////////////////////////////////

template<typename Stream, int L, int... Alpha, int B, int... Beta, int... Ts>
void dump_situation(Stream& s, situation<L, int_sequence<Alpha...>, B, int_sequence<Beta...>, int_set<Ts...>>)
{
    s << def_debug_name(def<L>{}) << " -> ";
    ((s << def_debug_name(def<Alpha>{}) << ' '), ...);
    s << ". " << def_debug_name(def<B>{}) << ' ';
    ((s << def_debug_name(def<Beta>{}) << ' '), ...);
    s << "| ";
    ((s << def_debug_name(def<Ts>{}) << ' '), ...);
    s << std::endl;
}

template<typename Stream, int L, int... Alpha, int... Ts>
void dump_situation(Stream& s, situation<L, int_sequence<Alpha...>, nothing, int_sequence<>, int_set<Ts...>>)
{
    s << def_debug_name(def<L>{}) << " -> ";
    ((s << def_debug_name(def<Alpha>{}) << ' '), ...);
    s << ". | ";
    ((s << def_debug_name(def<Ts>{}) << ' '), ...);
    s << std::endl;
}

//////////////////////////////////////////////////////////

template<typename Stream, typename... S>
void dump_situations(Stream& s, situation_set<S...>)
{
    (void(dump_situation(s, S{})), ...);
}

//////////////////////////////////////////////////////////

template<typename Stream, typename Situations, int Nr>
void dump_state(Stream& s, state<Nr, Situations>)
{
    s << "STATE " << Nr << std::endl << std::endl;
    dump_situations(s, Situations{});
    s << std::endl;
}

template<typename Stream, typename... States>
void dump_states(Stream& s, states_set<States...>)
{
    (void(dump_state(s, States{})), ...);
}

//////////////////////////////////////////////////////////

template<int S>
class parser
{
public:
    int match(const std::string_view& sv)
    {
        return _match_function(sv, 0);
    }

    const char* t_name(int t_nr) const
    {
        return _t_names[t_nr];
    }

    const char* nt_name(int t_nr) const
    {
        return _nt_names[t_nr];
    }
    
    template<typename Stream>
    void dump(Stream& s)
    {
        dump_states(s, states{});
    }

private:
    using t_set = insert_to_int_set_t<terminal_set_t<S>, end_of_input_v>;
    using nt_set = nonterminal_set_t<S>;
    
    using start_state = make_state_t<0, closure_t<S, int_set<end_of_input_v>>>;

    using states = recursive_transitions_t<states_set<start_state>>;
    
    debug_names<t_set> _t_names;
    debug_names<nt_set> _nt_names;
    match_f _match_function = match_function_for_terminal_set<t_set>::f;
};


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


enum symbols 
{
    stmt, ex,
    o_plus, o_minus, o_mul, o_div, o_and, o_or, o_xor, 
    lpar, rpar, comma,
    one 
};

template<>
struct def<o_plus>
{
    using type = char_terminal<'+'>;
};

template<>
struct def<o_minus>
{
    using type = char_terminal<'-'>;
};

template<>
struct def<o_mul>
{
    using type = char_terminal<'*'>;
};

template<>
struct def<o_div>
{
    using type = char_terminal<'/'>;
};

template<>
struct def<o_and>
{
    using type = char_terminal<'&'>;
};

template<>
struct def<o_or>
{
    using type = char_terminal<'|'>;
};

template<>
struct def<o_xor>
{
    using type = char_terminal<'^'>;
};

template<>
struct def<lpar>
{
    using type = char_terminal<'('>;
};

template<>
struct def<rpar>
{
    using type = char_terminal<')'>;
};

template<>
struct def<comma>
{
    using type = char_terminal<','>;
};

template<>
struct def<one>
{
    using type = char_terminal<'1'>;
};

template<>
struct def<ex>
{
    using type = alt<
        is<ex, o_plus, ex>, 
        is<ex, o_minus, ex>, 
        is<ex, o_mul, ex>,
        is<ex, o_div, ex>,
        is<ex, o_and, ex>,
        is<ex, o_or, ex>,
        is<ex, o_xor, ex>,
        is<lpar, ex, rpar>,
        is<o_minus, ex>,
        is<one>
    >;

    static const char* debug_name() { return "ex"; }
};

template<>
struct def<stmt>
{
    using type = alt<
        is<ex>,
        is<ex, comma, stmt>
    >;

    static const char* debug_name() { return "stmt"; }
};

int main()
{
    parser<stmt> p;
    p.dump(std::cout);

    return 0;
}
