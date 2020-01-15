-module(ctx).

-export([set/3,
         get/2,
         get/3,
         new/0,
         background/0,
         deadline/1,
         deadline/2,
         done/1,
         with_value/2,
         with_value/3,
         with_values/1,
         with_deadline/1,
         with_deadline_after/2,
         with_deadline_after/3]).

-export_type([t/0]).

-record(ctx, {values :: #{term() => term()},
              deadline :: {timer:time(), timer:time()} | infinity | undefined}).
-type t() :: #ctx{}.

-spec new() -> t().
new() ->
    #ctx{values=#{}}.

-spec background() -> t().
background() ->
    #ctx{values=#{}}.

-spec set(t(), term(), term()) -> t().
set(Ctx=#ctx{values=Values}, Key, Value) ->
    Ctx#ctx{values=maps:put(Key, Value, Values)}.

-spec get(t(), term()) -> term().
get(#ctx{values=Values}, Key) ->
    maps:get(Key, Values).

-spec get(t(), term(), term()) -> term().
get(#ctx{values=Values}, Key, Default) ->
    maps:get(Key, Values, Default).

-spec with_value(t(), term(), term()) -> t().
with_value(Ctx=#ctx{values=Values}, Key, Value) ->
    Ctx#ctx{values=maps:put(Key, Value, Values)}.

-spec with_value(term(), term()) -> t().
with_value(Key, Value) ->
    #ctx{values=#{Key => Value}}.

-spec with_values(#{term() => term()}) -> t().
with_values(Values) ->
    #ctx{values=Values}.

-spec with_deadline({integer(), integer()} | undefined | infinity) -> t().
with_deadline(Deadline) ->
    #ctx{values=#{},
         deadline=Deadline}.

-spec with_deadline_after(integer(), erlang:time_unit()) -> t().
with_deadline_after(After, Unit) ->
    with_deadline_after(#ctx{values=#{}}, After, Unit).

-spec with_deadline_after(t(), integer(), erlang:time_unit()) -> t().
with_deadline_after(Ctx, After, Unit) ->
    Ctx#ctx{deadline=deadline(After, Unit)}.

-spec deadline(t()) -> {integer(), integer()} | undefined | infinity.
deadline(#ctx{deadline=Deadline}) ->
    Deadline.

-spec done(t()) -> boolean().
done(#ctx{deadline={Deadline, _}}) ->
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
