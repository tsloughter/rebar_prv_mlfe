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
    Ctx = get_context(SourceDir, Module, Line, hl_fn("")),
    cf:format("~!__~s/~!_c~s.alp~!!:~!c~p~!!~n"
              "  ~!B=> ~!WFailed to unify types ~!g~s~!! ~!Wand ~!r~s~!!.~n"
              "~n"
              "~s~n", [SourceDir, Module, Line,
                       fmt_type(TypeA), fmt_type(TypeB), Ctx]);

format_error(SourceDir, {parse_error, File, Line, Error}) ->
    SourceDir = filename:dirname(File),
    Module = filename:rootname(filename:basename(File)),
    {PError, H} = format_p_error(Error),
    Ctx = get_context(SourceDir, Module, Line, hl_fn(H)),
    cf:format("~!__~s/~!_c~s.alp~!!:~!c~p~!!~n"
              "  ~!B=> ~!!Parser error: ~!W~s.~n"
              "~n"
              "~s~n", [SourceDir, Module, Line, PError, Ctx]);

format_error(SourceDir, Reason) ->
    io_lib:format("~s: ~p", [SourceDir, Reason]).

format_p_error({syntax_error,[]}) ->
    {cf:format("syntax error near ~!rthe end of the line~!!"), ""};

format_p_error({syntax_error,[Offender]}) ->
    O = unquote(Offender),
    {cf:format("syntax error near '~!r~s~!!~!W'", [O]), O};

format_p_error(E) ->
    io_lib:format("~p", [E]).

unquote([$" | R]) ->
    unquote(R, []).

unquote([$"], Acc) ->
    lists:reverse(Acc);
unquote([], Acc) ->
    lists:reverse(Acc);
unquote([C | R], Acc) ->
    unquote(R, [C | Acc]).

fmt_type({t_arrow, L, R}) ->
    io_lib:format("~s -> ~s", [fmt_type(L), fmt_type(R)]);
fmt_type(T) ->
    io_lib:format("~w", [T]).

hl_fn("") ->
    fun(X) ->
            X
    end;
hl_fn(O) ->
    P = re:replace(O, "[.^$*+?()[{\\\|\s#]", "\\\\&", [global]),
    R = list_to_binary(cf:format("~!r~s", [O])),
    fun(L) ->
            re:replace(L, ["(.*)", P, "(.*?)$"], ["\\1", R, "\\2"])
    end.

get_context(SourceDir, Module, Target, Fn) ->
    case file:open(io_lib:format("~s/~s.alp", [SourceDir, Module]),
                   [read, binary]) of
        {ok, Device} ->
            read_lines(Device, 1, Target, Fn, []);
        E ->
            ""
    end.

-define(AREA, 2).

read_lines(Device, Line, Target, Fn, Acc)
  when Line < Target - ?AREA ->
    case io:get_line(Device, "") of
        eof ->
            file:close(Device),
            lists:reverse(Acc);
        _Txt ->
            read_lines(Device, Line + 1, Target, Fn, Acc)
    end;
read_lines(Device, Line, Target, Fn, Acc)
  when Line > Target + ?AREA ->
    file:close(Device),
    lists:reverse(Acc);

read_lines(Device, Line, Target, Fn, Acc) ->
    case io:get_line(Device, "") of
        eof ->
            file:close(Device),
            lists:reverse(Acc);
        Txt ->
            L1 = case Line of
                     Target ->
                         cf:format("~!r~4b~!!: ~s", [Line, Fn(Txt)]);
                      _ ->
                         cf:format("~!c~4b~!!: ~s", [Line, Txt])
                  end,
            read_lines(Device, Line + 1, Target, Fn, [L1 | Acc])
    end.


