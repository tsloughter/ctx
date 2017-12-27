-module(ctx).

-export([set/3,
         get/2,
         get/3,
         background/0,
         deadline/2,
         done/1,
         with_value/2,
         with_values/1,
         with_deadline/1,
         with_deadline_after/2]).

-export_type([ctx/0]).

-type ctx() :: #{values := #{term() => term()},

                 deadline => {timer:time(), timer:time()} | infinity}.

-spec background() -> ctx().
background() ->
    #{values => #{}}.

-spec set(ctx(), term(), term()) -> ctx().
set(Ctx=#{values := Values}, Key, Value) ->
    Ctx#{values => maps:put(Key, Value, Values)}.

-spec get(ctx(), term()) -> ctx().
get(#{values := Values}, Key) ->
    maps:get(Key, Values).

-spec get(ctx(), term(), term()) -> term().
get(#{values := Values}, Key, Default) ->
    maps:get(Key, Values, Default).

-spec with_value(term(), term()) -> ctx().
with_value(Key, Value) ->
    #{values => #{Key => Value}}.

-spec with_values(#{term() => term()}) -> ctx().
with_values(Values) ->
    #{values => Values}.

-spec with_deadline(integer()) -> ctx().
with_deadline(Deadline) ->
    #{values => #{},
      deadline => Deadline}.

-spec with_deadline_after(integer(), erlang:time_unit()) -> ctx().
with_deadline_after(After, Unit) ->
    with_deadline_after(#{values => #{}}, After, Unit).

-spec with_deadline_after(ctx(), integer(), erlang:time_unit()) -> ctx().
with_deadline_after(Ctx, After, Unit) ->
    Ctx#{deadline => deadline(After, Unit)}.

done(#{deadline := {Deadline, _}}) ->
    erlang:monotonic_time() =< Deadline;
done(_) ->
    false.

%% internal

deadline(After, Unit) ->
    Offset = erlang:time_offset(),
    ConvertedTime = erlang:convert_time_unit(After, Unit, native),
    {erlang:monotonic_time() + ConvertedTime, Offset}.

%% start_deadline_timer(Time) ->
%%     erlang:start_timer(Time, self(), deadline_reached).
