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
    genserver:start(ServerAtom, initialServerState(), fun handle/2).

% Terminates all channels
handle(St, terminate_channels) ->
    lists:foreach(fun(Channel) -> 
        genserver:stop(list_to_atom(Channel))
    end, St#serverstate.channels),
    {reply, ok, St};

% Handle join request
handle(St, {join, Channel, UserPid}) ->
    case lists:member(Channel, St#serverstate.channels) of
        true -> 
            % Channel already exists,
            % join it!
            R = genserver:request(list_to_atom(Channel), {join, UserPid}), % Send join request to the correct Channel process
            {reply, R, St};
        false -> 
            % Channel does not exists => create it
            NewChannels = [Channel | St#serverstate.channels],
            UpdatedState = St#serverstate{channels = NewChannels}, % Updates state with the new channels

            % Start the Channel process and add the UserPid to state
            genserver:start(list_to_atom(Channel), initialChannelState(UserPid), fun channelHandler/2),
            {reply, ok, UpdatedState}
    end.

% Handle leave request
channelHandler(St, {leave, UserPid}) ->
    case lists:member(UserPid, St#channelstate.users) of
        true -> 
            % User is in the channel, remove it
            NewUsers = lists:delete(UserPid, St#channelstate.users),
            UpdatedState = St#channelstate{users = NewUsers}, % Update the state with the new user

            case length(St#channelstate.users) of
                0 -> 
                    % No users left in the channel, remove it
                    genserver:stop(self()), % Stop the channel process  
                    {reply, ok, UpdatedState};
                _ -> 
                    {reply, ok, UpdatedState}
            end,
            {reply, ok, UpdatedState};
        false -> 
            {reply, {error, user_not_joined, "User is not in the channel"}, St}
    end;

% Handle message send request
channelHandler(St, {join, UserPid}) ->
    case lists:member(UserPid, St#channelstate.users) of
        true -> 
            {reply, {error, user_already_joined, "User has already joined this channel"}, St};
        false -> 
            % User is not already joined => add it to the channel
            NewUsers = [UserPid | St#channelstate.users],
            UpdatedState = St#channelstate{users = NewUsers}, % Update the state with the new user

            {reply, ok, UpdatedState}
    end;

% Handle message send request
channelHandler(St, {message_send, Channel, Nick, UserPid, Msg}) ->    
    case lists:member(UserPid, St#channelstate.users) of
        true -> 
            spawn(fun() -> broadcast_message(St#channelstate.users, Channel, Nick, Msg, UserPid) end),
            {reply, ok, St};
        false -> 
            {reply, {error, user_not_joined, "User is not in the channel"}, St}
    end.

% Broadcast message to all users in the channel
broadcast_message(Users, Channel, Nick, Msg, SenderPid) ->
    Receivers = lists:delete(SenderPid, Users),
    lists:foreach(fun(User) -> 
            genserver:request(User, {message_receive, Channel, Nick, Msg})
    end, Receivers).
    
% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:request(ServerAtom, terminate_channels),
    genserver:stop(ServerAtom).

