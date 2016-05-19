-module(simple_semaphore).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
  start_link/2,
  start_link/1,
  new_task/2,
  new_task/1,
  register_worker/3,
  done/2,
  change_max_tasks/2,
  stop/1
]).

-record(st, {
  max_tasks :: pos_integer(),
  tasks :: ets:tid(),
  workers :: ets:tid(),
  current_tasks = 0 :: integer()
}).

-type task_ref()::term().

%% ===================================================================
%% API
%% ===================================================================

-spec start_link(atom(), pos_integer()) -> {ok, pid()} | {error, term()}.
start_link(Name, MaxTasks) ->
  gen_server:start_link({local, Name}, ?MODULE, [MaxTasks], []).

-spec start_link(pos_integer()) -> {ok, pid()} | {error, term()}.
start_link(MaxTasks) ->
  gen_server:start_link(?MODULE, [MaxTasks], []).

-spec new_task(pid(), pid()) -> {ok, task_ref()} | {error, limit_reached}.
new_task(Pid, WorkerPid) ->
  gen_server:call(Pid, {new_task, WorkerPid}).

-spec new_task(pid()) -> {ok, task_ref()} | {error, limit_reached}.
new_task(Pid) ->
  new_task(Pid, self()).

-spec register_worker(pid(), task_ref(), pid()) -> ok.
register_worker(Pid, TaskRef, WorkerPid) ->
  gen_server:call(Pid, {register_worker, TaskRef, WorkerPid}).

-spec done(pid(), task_ref()) -> ok.
done(Pid, TaskRef) ->
  gen_server:call(Pid, {done, TaskRef}).

-spec change_max_tasks(pid(), pos_integer()) -> ok.
change_max_tasks(Pid, NewMaxTasks) ->
  gen_server:call(Pid, {change_max_tasks, NewMaxTasks}).

-spec stop(pid()) -> term().
stop(Pid) ->
  gen_server:stop(Pid).

%% ===================================================================
%% GEN SERVER
%% ===================================================================

init([MaxTasks]) ->
  St = #st{
    max_tasks = MaxTasks,
    tasks = ets:new(tasks, [bag]),
    workers = ets:new(workers, [])
  },
  {ok, St}.

terminate(_Reason, _St) ->
  ok.

handle_cast(Msg, St) ->
  {stop, {unknown_cast, Msg}, St}.

handle_call({new_task, WorkerPid}, _From, St) ->
  {Reply, NewSt} = do_new_task(WorkerPid, St),
  {reply, Reply, NewSt};
handle_call({register_worker, TaskRef, WorkerPid}, _From, St) ->
  add_worker(TaskRef, WorkerPid, St),
  {reply, ok, St};
handle_call({done, TaskRef}, _From, St) ->
  NewSt = do_done(TaskRef, St),
  {reply, ok, NewSt};
handle_call({change_max_tasks, NewMaxTasks}, _From, St) ->
  {reply, ok, St#st{max_tasks = NewMaxTasks}};
handle_call(Msg, _From, St) ->
  {stop, {unknown_call, Msg}, St}.

handle_info({'DOWN', _Ref, process, WorkerPid, _Reason}, St) ->
  NewSt = on_worker_down(WorkerPid, St),
  {noreply, NewSt};
handle_info(_Info, St) ->
  {noreply, St}.

code_change(_OldVsn, St, _Extra) -> {ok, St}.

%%%-----------------------------------------------------------------------------
%%% PRIVATE
%%%-----------------------------------------------------------------------------

do_new_task(_WorkerPid, #st{max_tasks = MaxTasks, current_tasks = MaxTasks} = St) ->
  {{error, limit_reached}, St};
do_new_task(WorkerPid, #st{current_tasks = CurrentTasks} = St) ->
  TaskRef = make_ref(),
  add_worker(TaskRef, WorkerPid, St),
  {{ok, TaskRef}, St#st{current_tasks = CurrentTasks + 1}}.

do_done(TaskRef, #st{tasks = Tasks} = St) ->
  case ets:lookup(Tasks, TaskRef) of
    [] ->
      St;
    Results ->
      [remove_worker(WorkerPid, MonitorRef, St) || {_, {WorkerPid, MonitorRef}} <- Results],
      ets:delete(Tasks, TaskRef)
  end,
  decrement_tasks(St).

on_worker_down(WorkerPid, #st{tasks = Tasks, workers = Workers} = St) ->
  case ets:lookup(Workers, WorkerPid) of
    [{_, {TaskRef, _}}] ->

      ets:delete(Workers, WorkerPid),
      ets:delete_object(Tasks, {TaskRef, WorkerPid}),

      case ets:lookup(Tasks, TaskRef) of
        [] ->
          decrement_tasks(St);
        _ ->
          St
      end;

    [] ->
      St
  end.

add_worker(TaskRef, WorkerPid, #st{tasks = Tasks, workers = Workers}) ->
  MonitorRef = erlang:monitor(process, WorkerPid),
  ets:insert(Tasks, {TaskRef, WorkerPid}),
  ets:insert(Workers, {WorkerPid, {TaskRef, MonitorRef}}).

remove_worker(WorkerPid, MonitorRef, #st{workers = Workers}) ->
  erlang:demonitor(MonitorRef),
  ets:delete(Workers, WorkerPid).

decrement_tasks(#st{current_tasks = CurrentTasks} = St) ->
  St#st{current_tasks = max(0, CurrentTasks - 1)}.