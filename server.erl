-module(server).
-export([start/1,stop/1]).

-record(serverstate, {
    channels
}).

initialServerState() -> 
    #serverstate{
        channels = []
    }.

-record(channelstate, {
    users
}).

initialChannelState(UserPid) -> 
    #channelstate{
        users = [UserPid] % List of pids in channel
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    genserver:start(ServerAtom, initialServerState(), fun handle/2).


handle(St, {join, Channel, UserPid}) ->

    case lists:member(Channel, St#serverstate.channels) of
        true -> 
            % Channel already exists,
            % join it!
            R = genserver:request(list_to_atom(Channel), {join, UserPid}), % Send join request to the correct Channel process
            io:format("Channels1: ~p~n", [St#serverstate.channels]), % This will be one iteration behind. 
            io:format("R: ~p~n", [R]), % This will be one iteration behind. 
             

            {reply, R, St};
        false -> 
            % Channel does not exists => create it
            NewChannels = [Channel | St#serverstate.channels],
            UpdatedState = St#serverstate{channels = NewChannels}, % Updates state with the new channels

            % Start the Channel process and add the UserPid to state
            S = genserver:start(list_to_atom(Channel), initialChannelState(UserPid), fun channelHandler/2),

            io:format("Channels2: ~p~n", [St#serverstate.channels]), % This will be one iteration behind. 
            io:format("start: ~p~n", [S]),

            {reply, ok, UpdatedState}
    end.

channelHandler(St, {join, UserPid}) ->
    case lists:member(UserPid, St#channelstate.users) of
        true -> 
            {reply, {error, user_already_joined, "User has already joined this channel"}, St};
        false -> 
            % User is not already joined => add it to the channel
            NewUsers = [UserPid | St#channelstate.users],
            UpdatedState = St#channelstate{users = NewUsers}, % Update the state with the new user

            io:format("Channel users: ~p~n", [St#channelstate.users]), % This will be one iteration behind. 
            
            {reply, ok, UpdatedState}
    end.


    % Distinction work? We can just use Pid and then people can have the same nick for now

    % case lists:member(User, St#serverstate.users) of
    %     true -> 
    %         % User already exists, do nothing
    %         {reply, ok, St};
    %     false -> 
    %         % This is where we add users to our state? Maybe do this somewhere else when we first start? 
    %         NewUsers = [User | St#serverstate.users],
    %         UpdatedUsersState = St#serverstate{channels = NewUsers}, % Update the state with the new channels
    %         io:format("This is users: ~p~n", [St#serverstate.users]), % Print a message indicating a client joined the channel 
    %         {reply, ok, UpdatedUsersState}
    % end.
    
% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    Pid = whereis(ServerAtom),
case Pid of
    %ett till case som felhanterar
    _ ->
        unregister(ServerAtom),
        exit(ServerAtom, normal)
end.