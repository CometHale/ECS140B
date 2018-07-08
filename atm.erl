-module(atm).
-export([loop/0]).



loop() -> receive
% We'll test your code by sending messages 
% from the console to the bank and ATMprocesses.  
% These messages are operations to be performed by theseconcurrent processes. 
% The ATM processes will send messages to the bank process when necessary.  
% Similarly, the bank process will send messages to an ATM when necessary.

% The ATMs do not know anything about the user accounts.  
% Any given ATM keeps track of how much cash is available in that ATM. 

% one table per atm -- b/c don't know how many times start will be called
% {cash_supply, ATMCashSupply}
% {BankProcessID, BankName}


%tell the ATM processes to start up with $200 in one ATM and % $500 in the other
% start {start, BANK_PROCESS_ID, bank_name, atm_balance, atm_name}
	{start, "", _, _, _} ->
				io:format("ATM not created --- bank process id required~n"),
				loop();  
	% {start, _, "", _, _} ->
	% 			io:format("ATM not created --- bank name required~n"),
	% 			loop();
	{start, _, _, ATMBalance, _}  when ATMBalance < 0 ->
				io:format("ATM not created --- ATM balance must be at least 0.~n"),
				loop(); 
	{start, _, _, _, ""} ->
				io:format("ATM not created --- atm name required~n"),
				loop();
	{start, BankProcessID, BankName, ATMBalance, ATMName} ->
				ets:new(ATMName, [named_table]),
				ets:insert(ATMName,{cash_supply,ATMBalance}),
				ets:insert(ATMName,{bank, BankName,BankProcessID}),
				io:format("atm ~p started with ~p dollars cash available~n",[ATMName, ATMBalance]),
				loop();

% prints out how much cash an atm has
% "atm [atm_name] has [cash supply] dollars on hand" printed by the ATM
% cashsupply {cashsupply, atm_name}
% tm atm1 has 200 dollars on hand
	{cashsupply, ""} -> 
				io:format("ATM cashsupply not returned --- atm name required~n"),
				loop();
	{cashsupply, ATMName} -> 
				[H|_] = ets:lookup(ATMName,cash_supply), 
				{_, Balance} = H,
				io:format("atm ~p has ~p dollars on hand~n", [ATMName, Balance]), 
				loop();

% sends message to the Bank process to tell it to update bank_account_name's account
% with the given deposit amount and prints:
% "account [bank_account_name] now has [bank_account_balance]"
% bank sends confirmation to atm. ATM DOES NOT UPDATE IT'S CASHSUPPLY
% deposit {deposit, bank_account_name, deposit_amount, atm_name}
	{deposit, "", _, _} -> 
				io:format("cannot deposit --- bank account name required~n"),
				loop();
	{deposit, _, DepositAmount, _}  when DepositAmount < 0 ->
				io:format("cannot deposit --- deposit amount must be at least 0.~n"),
				loop(); 
	{deposit, _, _, ""}  ->
				io:format("cannot deposit --- atm name required~n"),
				loop();
	{deposit, BankAccountName, DepositAmount, ATMName} ->
		[H|_] = ets:lookup(ATMName,bank),
		{_, BankName, BPID} = H,
		BPID ! {deposit, BankAccountName, DepositAmount, BankName, self()},
		loop();
	{deposit, _} -> loop(); % for deposit, atm doesn't do anything regardless of
	% whether the Bank accepts the deposit.

% sends message to the Bank process to tell it to update bank_account_name's account
% with the given withdrawal amount. It also displays a message:
% "[withdrawal amount] dollars withdrawn"
% "account [account_name] now has [amount] dollars"
% bank sends confirmation message and atm allows withdraw -- atm updates its cashsupply
% "account [bank_account_name] now has [bank_account_balance]" which is printed by the bank

% IF THE ATM DOESN'T HAVE ENOUGH, IT DOESN'T SEND THE MESSAGE TO THE BANK
% IT HALTS AND PRINTS A MESSAGE:
% "sorry, insufficient cash in this atm"

% IF THE ACCOUNT DOESN'T HAVE ENOUGH MONEY, THE BANK HALTS THE TRANSACTION
%  AND PRINTS: "sorry, account [account_name] has only [balance] dollars"
% BANK HALTED THE TRANSACTION SO ATM DOESN'T LOSE ANY MONEY

% withdraw {withdraw, bank_account_name, deposit_amount, atm_name}

	{withdraw, "", _, _} -> 
				io:format("cannot deposit --- bank account name required~n"),
				loop();
	{withdraw, _, DepositAmount, _}  when DepositAmount < 0 ->
				io:format("cannot deposit --- deposit amount must be at least 0.~n"),
				loop(); 
	{withdraw, _, _, ""}  ->
				io:format("cannot deposit --- atm name required~n"),
				loop();
	{withdraw, BankAccountName, WithdrawAmount, ATMName} ->
		[H1|_] = ets:lookup(ATMName,bank),
		{_, BankName, BPID} = H1,
		[H2|_] = ets:lookup(ATMName,cash_supply), 
		{_, CashSupply} = H2,
		case WithdrawAmount > CashSupply of
			true ->
				io:format("sorry, insufficient cash in this atm"),
				loop();
			false ->
				BPID ! {withdraw, BankAccountName, WithdrawAmount, BankName, ATMName, self()},
				loop()
		end;

	{withdraw, check,  WithdrawAmount, ATMName, Result} ->
		case Result == true of
			true -> 
				[H|_] = ets:lookup(ATMName,cash_supply),
				{_, CashSupply} = H,
				NewBalance = CashSupply - WithdrawAmount,
				ets:update_element(ATMName,cash_supply , {2, NewBalance}),
				loop();
			false -> loop() % ets:update_element wasn't able to find the account at the bank, so do nothing
		end;
	
	_ ->   io:format("ATM:None of the patterns matched, so nothing's gonna happen!~n"), loop() % for everything else, there's mastercard--- an error 
end.





