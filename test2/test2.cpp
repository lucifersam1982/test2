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

template<int V>
using int_constant = std::integral_constant<int, V>;

//////////////////////////////////////////////////////////

template<bool B, typename T1, typename T2, typename Enable = void>
struct choose_type
{};

template<bool B, typename T1, typename T2>
struct choose_type<B, T1, T2, std::enable_if_t<B>>
{
    using type = T1;
};

template<bool B, typename T1, typename T2>
struct choose_type<B, T1, T2, std::enable_if_t<!B>>
{
    using type = T2;
};

template<bool B, typename T1, typename T2>
using choose_type_t = typename choose_type<B, T1, T2>::type;

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
    static int_constant<-1> foo(...);

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
constexpr bool nullable_sequence = nullable_def<Seq, int_set<>>::value;

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
{};

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
    using type = situation_set<choose_type_t<I == V, update_situation_t<S, TSet>, S>...>;
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

template<typename... S>
struct situations
{};

//////////////////////////////////////////////////////////

template<typename Set, typename Situations>
struct add_situations
{};

template<typename Set, typename Situations>
using add_situations_t = typename add_situations<Set, Situations>::type;

template<typename Set, typename First, typename... Rest>
struct add_situations<Set, situations<First, Rest...>>
{
    using type = add_situations_t<
        insert_situation_to_set_t<Set, First>,
        situations<Rest...>
    >;
};

template<typename Set>
struct add_situations<Set, situations<>>
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
    using type = situations<
        typename seq_closure<V, Seq, TSet>::type...
    >;
};

template<int V, int... Seq, typename TSet>
struct def_closure<V, is<Seq...>, TSet>
{
    using type = situations<
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
    using type = situations<>;
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
        choose_type_t<
            nullable_sequence<is<Beta...>>,
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
    using type = situations<>;
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

template<typename Situations>
struct root_situations
{};

template<typename... S>
struct root_situations<situations<S...>>
{
    using type = situation_set<S...>;
};

template<typename Situations>
using root_situations_t = typename root_situations<Situations>::type;

//////////////////////////////////////////////////////////

template<int Nr, typename Situations>
struct state
{};

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

template<typename First, typename... Rest>
inline int match(const std::string_view& sv, int x)
{
    const matcher<First> m;
    if (m(sv))
        return x;
    if constexpr (sizeof...(Rest) > 0)
        return match<Rest...>(sv, x + 1);
    else
        return -1;
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
        dump_state(s, start_state{});
    }

private:
    using t_set = insert_to_int_set_t<terminal_set_t<S>, end_of_input_v>;
    using nt_set = nonterminal_set_t<S>;
    
    using start_state = state<
        0,
        recursive_closure_t<
            root_situations_t<
                closure_t<S, int_set<end_of_input_v>>
            >
        >
    >;
    
    debug_names<t_set> _t_names;
    debug_names<nt_set> _nt_names;
    match_f _match_function = match_function_for_terminal_set<t_set>::f;
};


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


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

    static const char* debug_name() { return "opt"; }
};

template<>
struct def<ex>
{
    using type = alt<is<ex, plus, ex>, is<ex, minus, ex>, is<one>>;

    static const char* debug_name() { return "ex"; }
};

int main()
{
    parser<ex> p;
    p.dump(std::cout);

    return 0;
}
