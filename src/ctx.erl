-module(ctx).

-export([set/3,
         get/2,
         get/3,
         values/1,
         new/0,
         background/0,
         deadline/1,
         deadline/2,
         done/1,
         with_value/2,
         with_value/3,
         with_values/1,
         with_deadline/1,
         with_deadline/2,
         with_deadline_after/2,
         with_deadline_after/3,
         time_to_deadline/1,
         time_to_deadline/2]).

-export_type(
   [t/0,
    ctx/0]).

-record(ctx, {values :: #{term() => term()},
              deadline :: {non_neg_integer(), non_neg_integer()} | infinity | undefined}).

-type t() :: #ctx{}.
-type ctx() :: #ctx{}.

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

-spec values(t()) -> map().
values(#ctx{values=Values}) ->
    Values.

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

-spec with_deadline(t(), {integer(), integer()} | undefined | infinity) -> t().
with_deadline(Ctx, Deadline) ->
    Ctx#ctx{deadline=Deadline}.

-spec with_deadline_after(integer(), erlang:time_unit()) -> t().
with_deadline_after(After, Unit) ->
    with_deadline_after(#ctx{values=#{}}, After, Unit).

-spec with_deadline_after(t(), integer(), erlang:time_unit()) -> t().
with_deadline_after(Ctx, After, Unit) ->
    Ctx#ctx{deadline=deadline(After, Unit)}.

-spec deadline(t()) -> {integer(), integer()} | undefined | infinity.
deadline(#ctx{deadline=Deadline}) ->
    Deadline.

-spec time_to_deadline(t()) -> integer() | undefined | infinity.
time_to_deadline(Ctx) ->
    case deadline(Ctx) of
        undefined ->
            undefined;
        infinity ->
            infinity;
        {Deadline, _} ->
            Deadline - erlang:monotonic_time()
    end.

-spec time_to_deadline(t(), erlang:time_unit()) -> integer() | undefined | infinity.
time_to_deadline(Ctx, Unit) ->
    case deadline(Ctx) of
        undefined ->
            undefined;
        infinity ->
            infinity;
        {Deadline, _} ->
            TTD = Deadline - erlang:monotonic_time(),
            erlang:convert_time_unit(TTD, native, Unit)
    end.

-spec done(t()) -> boolean().
done(#ctx{deadline={Deadline, _}}) ->
    erlang:monotonic_time() >= Deadline;
done(_) ->
    false.

%% internal

deadline(After, Unit) ->
    Offset = erlang:time_offset(),
    ConvertedTime = erlang:convert_time_unit(After, Unit, native),
    {erlang:monotonic_time() + ConvertedTime, Offset}.

%% start_deadline_timer(Time) ->
%%     erlang:start_timer(Time, self(), deadline_reached).
