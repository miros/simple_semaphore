-module(simple_semaphore_SUITE).

-export([all/0,
         group/1,
         groups/0,

         init_per_group/2,
         end_per_group/2,

         init_per_testcase/2,
         end_per_testcase/2]).

-export([
  explicit_done/1,
  implicit_done/1,
  register_worker/1
]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
  [{group, main}].

group(main) ->
  [{timetrap, {seconds, 5}}].

groups() ->
  [
   {main,
    [],
    [
      explicit_done,
      implicit_done,
      register_worker
    ]
  }
 ].

init_per_group(_, Config) ->
  Config.

end_per_group(_, _Config) ->
  ok.

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, _Config) ->
  ok.


%%%===================================================================
%%% Test cases
%%%===================================================================

explicit_done(_Config) ->
  {ok, Semaphore} = simple_semaphore:start_link(2),

  {ok, Ref1} = simple_semaphore:new_task(Semaphore),
  {ok, _} = simple_semaphore:new_task(Semaphore),
  {error, limit_reached} = simple_semaphore:new_task(Semaphore),

  simple_semaphore:done(Semaphore, Ref1),
  {ok, _} = simple_semaphore:new_task(Semaphore),

  simple_semaphore:stop(Semaphore),

  ok.

implicit_done(_Config) ->
  {ok, Semaphore} = simple_semaphore:start_link(1),

  WorkerPid = spawn(fun() ->
    {ok, _} = simple_semaphore:new_task(Semaphore),
    timer:sleep(infinity)
  end),
  timer:sleep(1),

  {error, limit_reached} = simple_semaphore:new_task(Semaphore),

  exit(WorkerPid, kill),
  timer:sleep(1),

  {ok, _} = simple_semaphore:new_task(Semaphore),

  simple_semaphore:stop(Semaphore),

  ok.

register_worker(_Config) ->
  {ok, Semaphore} = simple_semaphore:start_link(1),

  WorkerPid1 = spawn(fun() ->
    timer:sleep(infinity)
  end),

  WorkerPid2 = spawn(fun() ->
    timer:sleep(infinity)
  end),

  {ok, Ref1} = simple_semaphore:new_task(Semaphore, WorkerPid1),
  ok = simple_semaphore:register_worker(Semaphore, Ref1, WorkerPid2),

  exit(WorkerPid1, kill),
  timer:sleep(1),

  {error, limit_reached} = simple_semaphore:new_task(Semaphore),

  exit(WorkerPid2, kill),
  timer:sleep(1),

  {ok, _} = simple_semaphore:new_task(Semaphore),

  simple_semaphore:stop(Semaphore),

  ok.