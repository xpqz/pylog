% Stream property definitions for ISO I/O
% This file provides Prolog-level definitions that support backtracking

% stream_property/2 - backtrackable stream property access
% This delegates to a builtin that returns a list of properties
stream_property(Stream, Property) :-
    '$stream_properties'(Stream, Properties),
    member(Property, Properties).

% Helper to enumerate properties
% The builtin $stream_properties/2 returns a list of all properties