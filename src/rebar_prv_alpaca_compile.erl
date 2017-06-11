-module(rebar_prv_alpaca_compile).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(NAMESPACE, alpaca).
-define(DEPS, [{default, lock}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {namespace, ?NAMESPACE},
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 alpaca compile"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Alpaca rebar3 compiler plugin"},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
                  undefined ->
                      rebar_state:project_apps(State);
                  AppInfo ->
                      [AppInfo]
              end,
    TestsEnabled = [P || P <- rebar_state:current_profiles(State), P == test],
    case compile_apps(Apps, TestsEnabled, State, []) of
        [] ->
            {ok, State};
        Errors ->
            io:format("~s", [Errors]),
            {error, "error"}
    end.

compile_apps([], _TestsEnabled, State, Errors) ->
    Errors;

compile_apps([AppInfo | Apps], TestsEnabled, State, Errors) ->
    EBinDir = rebar_app_info:ebin_dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),
    SourceDir = filename:join(rebar_app_info:dir(AppInfo), "src"),
    Info = rebar_dir:src_dirs(Opts),

    FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.alp\$"),
    Deps = rebar_state:all_deps(State),

    AllFoundFiles = FoundFiles ++ lists:flatmap(fun gather_files/1, Deps),

    case alpaca:compile({files, AllFoundFiles}, TestsEnabled) of
        {ok, Compiled} ->
            [file:write_file(filename:join(EBinDir, FileName), BeamBinary) ||
                {compiled_module, ModuleName, FileName, BeamBinary} <- Compiled],
            compile_apps(Apps, TestsEnabled, State, Errors);
        {error, Reason} ->
            Error = format_error(SourceDir, Reason),
            compile_apps(Apps, TestsEnabled, State, [Error | Errors])
    end.

gather_files(AppInfo) ->
    SourceDir = filename:join(rebar_app_info:dir(AppInfo), "src"),
    rebar_utils:find_files(SourceDir, ".*\\.alp\$").

-spec format_error( any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec format_error(string(), any()) ->  iolist().
format_error(SourceDir, {cannot_unify, Module, Line, TypeA, TypeB}) ->
    cf:format("~!__~s/~!_c~s.alp~!!:~!c~p~!! "
              "Failed to unify types ~!g~s~!! and ~!r~s~!!.~n", [SourceDir, Module, Line, TypeA, TypeB]);
format_error(SourceDir, Reason) ->
    io_lib:format("~s: ~p", [SourceDir, Reason]).
