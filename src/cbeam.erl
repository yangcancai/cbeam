-module(cbeam).

-export([main/0, main/1]).
-export([run/1, run/2, list_dirs/1, list_dirs_beam/1, mkdir/1]).

%% @doc For running with:
%% erl +sbtu +A1 -noinput -mode minimal -boot start_clean -s rebar3 main -extra "$@"
-spec main() -> no_return().
main() ->
    List = init:get_plain_arguments(),
    main(List).

main([BeamFile]) ->
    Content = run(BeamFile),
    ok = file:write_file(<<(erlang:list_to_binary(BeamFile))/binary, ".erl">>, Content);
main([EbinDir, OutErlDir]) ->
    run(EbinDir, OutErlDir);
main(_) ->
    io:format("\ncbeam is a tool for working with Erlang projects.\n\n\n"),
    io:format("Usage: ~n"),
    io:format("~-50s~s~n", ["cbeam InEbinDir OutErlDir", "Decode all beam to OutErlDir"]),
    io:format("~-50s~s~n", ["cbeam BeamFile", "Show erl on shell"]),
    ok.

%% 反编译mod.beam为erl
run(Mod) when is_atom(Mod) ->
    run(code:which(Mod));
run(Mod) when is_binary(Mod)->
  run(erlang:binary_to_list(Mod));
run(Mod) ->
    {ok, {_, [{abstract_code, {_, Ac}}]}} = beam_lib:chunks(Mod, [abstract_code]),
    io_lib:format("~s~n",
                  [unicode:characters_to_binary(erl_prettypr:format(erl_syntax:form_list(Ac)))]).

%% 搜索EbinDir下的beam, 编译出的erl写入DstDir
%% DstDir和EbinDir同一个级别目录
run(EbinDir, DstDir) ->
    %% 列出所有文件夹和所有*.beam文件
    Files = list_dirs_beam(EbinDir),
    run(Files, EbinDir, DstDir).

run([], _EbinDir, _DstDir) ->
    ok;
run([File | Rest], EbinDir, DstDir) ->
    Content = run(File),
    %% 目录替换
    ErlFile =
        lists:flatten(
            string:replace(File, EbinDir, DstDir)),
    ErlFile1 =
        lists:flatten(
            string:replace(ErlFile, ".beam", ".erl")),
    mkdir(ErlFile1),
    ok = file:write_file(ErlFile1, Content),
    run(Rest, EbinDir, DstDir).

list_dirs_beam(Dir) ->
    [File || File <- list_dirs(Dir), is_beam(File)].

list_dirs(Dir) ->
    list_dirs_r([Dir], []).

list_dirs_r([] = _Files, Acc) ->
    Acc;
list_dirs_r([File | Tail] = _Files, Acc) ->
    case filelib:is_dir(File) of
        true ->
            case file:list_dir(File) of
                {ok, NewFiles} ->
                    FullNewFiles = [filename:join(File, N) || N <- NewFiles],
                    list_dirs_r(FullNewFiles ++ Tail, Acc);
                {error, Reason} ->
                    io:format("List dir(~p): ~p~n", [File, Reason]),
                    list_dirs_r(Tail, Acc)  % Ignore dir if error
            end;
        false ->
            list_dirs_r(Tail, [File | Acc])
    end.

is_beam(File) ->
    is_beam_(lists:reverse(File)).

is_beam_("maeb." ++ _) ->
    true;
is_beam_(_) ->
    false.

mkdir(Dir) ->
    mkdir(filename:dirname(Dir), "").

mkdir([], _Acc) ->
    ok;
mkdir(Dir, Acc) ->
    case string:split(Dir, "/") of
        [[], R] ->
            mkdir("/", R, Acc);
        [H, R] ->
            mkdir(H, R, Acc);
        [H] ->
            mkdir(H, [], Acc)
    end.

mkdir(H, R, Acc) ->
    Dir = case Acc of
              "" ->
                  H;
              _ ->
                  Acc ++ "/" ++ H
          end,
    case filelib:is_dir(Dir) of
        false ->
            file:make_dir(Dir);
        _ ->
            ignore
    end,
    mkdir(R, Dir).
