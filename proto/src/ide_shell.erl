-module(ide_shell).
-include_lib("wx/include/wx.hrl").
-compile(export_all).


start() ->
	loop([], 1, make_window()).
	
make_window() ->
	Server = wx:new(),
	Frame  = wxFrame:new(Server, -1, "Erlang Shell", [{size, {800, 300}}]),
	Panel  = wxPanel:new(Frame),
	
  % The style of the text box
	ShellTextBox = wxTextCtrl:new(Panel, 001, [{style, ?wxTE_MULTILINE}, 
											   {size, {800, 300}}]),
	wxWindow:setForegroundColour(ShellTextBox, ?wxWHITE),
	wxWindow:setBackgroundColour(ShellTextBox, ?wxBLACK),
	wxWindow:setFont(ShellTextBox, wxFont:new(12, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[])),
	
  % Set sizers
	MainSizer = wxBoxSizer:new(?wxVERTICAL),
	wxSizer:add(MainSizer, ShellTextBox, []),
	wxPanel:setSizer(Panel, MainSizer),
	
	wxFrame:show(Frame),
	
	wxTextCtrl:connect(ShellTextBox, char),
	
	ShellTextBox.
	
loop(Function, ArgNum, ShellTextBox) ->
	wxTextCtrl:writeText(ShellTextBox, integer_to_list(ArgNum) ++ "> "),
	receive
		#wx{id = 001, event = #wxKey{type = char}} ->
			io:format("BAM")
	end,
	loop([], 1, ShellTextBox).

		
	
	
	
