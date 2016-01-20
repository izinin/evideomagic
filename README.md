# evideomagic
This is erlang API wrapper for video processing utility libav. Client requests are 
accepted by one working process. When the process is busy with processing, pended requests
forms queue in the server memory. 

## Dependencies
    Open source audio and video processing tools: <https://libav.org/> 
    installation on ubuntu :
        apt-get install libav-tools

## Configuration
    The following settings supported in the application 
        {workdir, "/tmp/evideomagic"} -- directory to store temporary files

## API 
evideomagic performs requests asynchronically. To receive result in calling client process 
the API methods include parameter process id. Message sent to client process is a record,
according to command :


    command               | API method    |  record definition
:--------------------------|:-------------:|:---------------------------------
creating video thumbnail  | makeThumbnail | `rd(make_thumbnail, {fnamein, fnameout, pid}).`

## Manual run. 
    make run
    in erlang console:
    1> application:start(evideomagic).
    2> rd(make_thumbnail, {fnamein, fnameout, pid}).
    3> Pid = spawn_link(fun() -> receive #make_thumbnail{fnameout=Tmp} -> io:format("Thumbnail file : ~p", [Tmp]), exit(normal) end end).
    4> evideomagic:makeThumbnail("test/SampleVideo_1080x720_1mb.mp4", Pid).
    
    
## Using in code.
```
-include_lib("evideomagic/include/evideomagic.hrl").
...

upload_video(Object, Type, File) ->
    ...
    evideomagic:makeThumbnail(Object:data(), self()),
    receive 
        #make_thumbnail{fnameout=ThumbFname} -> {ok, ThumbBin} = file:read_file(ThumbFname), 
                                            Thumb = base64:encode(ThumbBin)
    end,
    ...
    
```
    


    
    
