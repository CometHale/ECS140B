-module(bank).
-export([loop/0]).

% We'll test your code by sending messages 
% from the console to the bank and ATMprocesses.  
% These messages are operations to be performed by theseconcurrent processes. 
% The ATM processes will send messages to the bank process when necessary.  
% Similarly, the bank process will send messages to an ATM when necessary.

% The bank keeps track of the user account information.  
%  The bank does not know how much cash is in any ATM.

% each bank_name will be the name of a table
% each row will be a tuple of the account_name and account_balance

loop() -> receive
        % create -- initialize a bank, EX: {create, [{account_name_1, account_balance_1},{account_name_2, account_balance_2},... ]}
        {create, [], BankName} -> 
                ets:new(BankName, [named_table]), %account list is empty, just create the bank table
                io:format("bank ~p created~n",[BankName]),
                loop(); 
        {create,_, ""} -> 
                io:format("bank not created -- a bank must have a name.~n"), % bank name is empty, print message [ERROR]
                loop();  
        {create, AccountList, BankName} -> 
                                ets:new(BankName,[named_table]), % everything A-OK
                                lists:map(fun(I) -> ets:insert(BankName,I) end, AccountList), % DO ERROR HANDLING: WHAT IF VALUE FOR ACCOUNT ISN'T A NUMBER
                                io:format("bank ~p created~n",[BankName]),
                                loop(); 

        % open --  add an additional account, EX: {open, account_name, account_balance, bank_name}
        {open, _, _, ""} -> 
                                io:format("account not opened -- a bank must have a name.~n"),
                                loop(); % bank name is empty/doesn't exist, print message [ERROR]
        % {open, AccountName, AccountBalance, BankName} -> ; % bank name is doesn't exist, print message [ERROR]
        {open, "", _, _} -> 
                                io:format("account not opened -- a bank must have a name.~n"),
                                loop(); %account name is empty
        {open, _, AccountBalance,_} when AccountBalance < 0 -> 
                                io:format("account not opened -- account balance must be at least 0.~n"),
                                loop(); % account balance is negative, print message [ERROR]
        % {open, AccountName, AccountBalance, BankName} when length(ets:lookup(BankName, AccountName)) > 0 ->
        %                       io:format("account not opened -- account name already taken.~n"),
        %                       loop(); % account name is already taken, print message [ERROR]
        {open, AccountName, AccountBalance, BankName} -> 
                                ets:insert(BankName, {AccountName, AccountBalance}), 
                                io:format("new account ~p opened with ~p dollars~n", [AccountName, AccountBalance]),
                                loop(); % everything A-OK

        % balance -- prints out the balance of an account at a particular bank, EX: {balance, account_name, bank_name}
        {balance, _, ""}    -> io:format("can't check balance -- please give a bank name.~n"), % bank name is empty, print message [ERROR]
                                loop();
        % {balance, AccountName, BankName} when length(ets:lookup(BankName, AccountName)) == 0 ->
        %                       io:format("can't check balance -- account doesn't exist.~n"),
        %                       loop(); %account name doesn't exist, print message [ERROR]
        {balance, AccountName, BankName} -> 
                [H|_] = ets:lookup(BankName, AccountName),
                {Name, Balance}= H,
                io:format("account ~p has ~p dollars~n", [Name, Balance]),
                loop(); %everything A-OK 
        
        % helpers
        % check_account_updated --- determines if the given account was updated and sends
        % confirmation to ATMProcess if it is provided. 
        % ets:update_element returns false if the provided key doesn't exist

        {check_account_updated, AccountName, AccountBalance, Result} ->
            case Result == true of
                true -> io:format("account ~p now has ~p~n",[AccountName, AccountBalance]),
                    loop();
                false -> io:format("account ~p does not exist. Please open an account first~n", [AccountName]),
                    loop()
            end;
        
        {check_account_updated, AccountName, AccountBalance, Result, ATMProcess} ->
            case Result == true of
                true ->
                    io:format("account ~p now has ~p~n",[AccountName, AccountBalance]),
                    ATMProcess ! {deposit, true},
                    loop();
                false ->
                    io:format("account ~p does not exist. Please open an account first~n",[AccountName]),
                    ATMProcess ! {deposit, false},
                    loop()
            end;

        {check_account_updated, AccountName, AccountBalance, Result, WithDrawAmount, ATMName, ATMProcess} ->
            case Result == true of
                true -> 
                    io:format("account ~p now has ~p~n",[AccountName, AccountBalance]),
                    ATMProcess ! {withdraw, check, WithDrawAmount, ATMName, true},
                    loop();
                false ->
                    io:format("account ~p does not exist. Please open an account first~n",[AccountName]),
                    ATMProcess ! {withdraw, check, WithDrawAmount, ATMName, false},
                    loop()
            end;

        % % deposit -- deposits money into a user's account and prints a message:
        % % "account [bank_account_name] now has [bank_account_balance]"
        % % bank sends confirmation to atm.
        
        % % deposit directly to bank
        {deposit, _, _, ""} ->
                io:format("deposit not initiated -- a bank name is required.~n"),
                loop(); %bank name is empty/doesn't exist, print message [ERROR]
        {deposit, "", _, _} ->
                io:format("deposit not initiated -- an account name is required.~n"),
                loop(); %account name doesn't exist, print message [ERROR]
        {deposit, _, DepositAmount, _} when DepositAmount < 0 ->
                io:format("deposit not initiated -- deposit amount must be at least 0.~n"),
                loop(); %deposit amount is negative, print message [ERROR]
        {deposit, AccountName, DepositAmount, BankName} ->
                [H|_] = ets:lookup(BankName, AccountName),
                {_, Balance}= H,
                NewBalance = Balance + DepositAmount,
                Res = ets:update_element(BankName, AccountName, {2, NewBalance}),
                self() ! {check_account_updated, AccountName, NewBalance, Res}, 
                loop(); % everything is A-Ok
        
        % % deposit through atm
        {deposit, _,_, "", _} ->
                io:format("deposit not initiated -- a bank name is required.~n"),
                loop(); %bank name is empty/doesn't exist, print message [ERROR]
        {deposit, "", _, _, _} ->
                io:format("deposit not initiated -- an account name is required.~n"),
                loop(); %account name empty, print message [ERROR]
        {deposit, _, DepositAmount, _, _} when DepositAmount < 0 ->
                io:format("deposit not initiated -- deposit amount must be at least 0.~n"),
                loop(); %deposit amount is negative, print message [ERROR]
        {deposit, AccountName, DepositAmount, BankName, ATMProcess} ->
                [H|_] = ets:lookup(BankName, AccountName),
                {_, Balance} = H,
                NewBalance = Balance + DepositAmount, 
                Res = ets:update_element(BankName, AccountName, {2, NewBalance}),
                self() ! {check_account_updated, AccountName, NewBalance, Res, ATMProcess},
                loop(); % everything is A-Ok
        

        % % withdraw -- withdraws money from a user's accoutn and prints a message:
        % % "account [bank_account_name] now has [bank_account_balance]"
        % % bank sends confirmation to atm.
        % % IF THE ACCOUNT DOESN'T HAVE ENOUGH MONEY, THE BANK HALTS THE TRANSACTION
        % %  AND PRINTS: "sorry, account [account_name] has only [balance] dollars"
        % % BANK HALTED THE TRANSACTION SO ATM DOESN'T LOSE ANY MONEY

        % % withdraw directly from bank
        {withdraw, _, _, ""} ->
                io:format("withdraw not initiated -- a bank name is required.~n"),
                loop(); %bank name is empty/doesn't exist, print message [ERROR]
        {withdraw, "", _, _} ->
                io:format("withdraw not initiated -- an account name is required.~n"),
                loop(); %account name doesn't exist, print message [ERROR]
        {withdraw, _, WithdrawAmount, _} when WithdrawAmount < 0 ->
                io:format("withdraw not initiated -- deposit amount must be at least 0.~n"),
                loop(); %deposit amount is negative, print message [ERROR]
        
        
        {withdraw, AccountName, WithdrawAmount, BankName} ->
                [H|_] = ets:lookup(BankName, AccountName),
                {_, Balance}= H,
                case Balance >= WithdrawAmount of
                    true -> 
                        NewBalance = Balance - WithdrawAmount,
                        Res = ets:update_element(BankName, AccountName, {2, NewBalance}),
                        self() ! {check_account_updated, AccountName, NewBalance, Res},
                        io:format(" ~p dollars withdrawn~n", [WithdrawAmount]),
                        loop(); % everything is A-Ok
                    false -> 
                        io:format("sorry, account ~p has only ~p dollars~n", [AccountName, Balance]),
                        loop()
                end;
    
        % withdraw through atm
        {withdraw, _,_, "", _, _} ->
                io:format("withdraw not initiated -- a bank name is required.~n"),
                loop(); %bank name is empty/doesn't exist, print message [ERROR]
        {withdraw, "", _, _, _, _} ->
                io:format("deposit not initiated -- an account name is required.~n"),
                loop(); %account name empty, print message [ERROR]
        {withdraw, _, WithdrawAmount, _, _, _} when WithdrawAmount < 0 ->
                io:format("deposit not initiated -- deposit amount must be at least 0.~n"),
                loop(); %deposit amount is negative, print message [ERROR]


        {withdraw, AccountName, WithdrawAmount, BankName,ATMName, ATMProcess} ->
                [H|_] = ets:lookup(BankName, AccountName),
                {_, Balance} = H,
                case Balance >= WithdrawAmount of
                    true -> 
                        NewBalance = Balance - WithdrawAmount,
                        Res = ets:update_element(BankName, AccountName, {2, NewBalance}),
                        self() ! {check_account_updated, AccountName, NewBalance, Res, WithdrawAmount, ATMName, ATMProcess},
                        io:format(" ~p dollars withdrawn~n", [WithdrawAmount]),
                        loop(); % everything is A-Ok
                    false -> 
                        io:format("sorry, account ~p has only ~p dollars~n", [AccountName, Balance]),
                        loop()
                end;


        _ ->   io:format("Bank:None of the patterns matched, so nothing's gonna happen!~n"), loop() % for everything else, there's mastercard--- an error 
    end.
