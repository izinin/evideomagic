%% @author igorzinin
%% @doc @todo Add description to evideomagic_test.


-module(evideomagic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("evideomagic.hrl").

thumbnail_test() ->
    {setup, fun() -> 
            Pid = spawn_link(fun() -> 
                    %TODO: unit test is broken. It does not check assert macro, 
                    % even finally temp file is deleted -- i.e. locigally the test case is correct
                    receive #make_thumbnail{fnameout=Tmp} -> 
                                {ok, Bin} = file:read_file(Tmp), 
                                Base64 = base64:encode(Bin),  
                                ?assert(binary:part(Base64, {1, 10}) == <<"VBORw0KGgo">>),
                                file:delete(Tmp),
                                exit(normal) 
                    end 
                 end),
            evideomagic:makeThumbnail("test/SampleVideo_1080x720_1mb.mp4", Pid)
        end}.


