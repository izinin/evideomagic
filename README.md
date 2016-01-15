# evideomagic
This is erlang API wrapper for video processing utility libav. 

## Dependencies
    Open source audio and video processing tools: https://libav.org/ 
    installation on ubuntu :
        apt-get install libav-tools

## manual run
    make run
    in erlang console:
    1> mnesia:create_schema([node()]).
    2> application:start(evideomagic).
    3> evideomagic:makeThumbnail("test/SampleVideo_1080x720_1mb.mp4", "test/out.png").
