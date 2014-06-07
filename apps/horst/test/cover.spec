%% List of Nodes on which cover will be active during test.
%% Nodes = [atom()]
{nodes, ['erlything1@macbook-pro', 'erlything@macbook-pro']}.       
%% Cover analysis level.
%% Level = details | overview
{level, details}.       
%% Directories to include in cover.
%% Dirs = [string()]
{incl_dirs, ["./apps/horst/ebin", "./apps/horst/src"]}.
%% Directories, including subdirectories, to include.
{incl_dirs_r, ["./apps/horst/include"]}.
{incl_mods, [actor_group]}.