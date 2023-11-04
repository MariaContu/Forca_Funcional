import System.IO

main :: IO ()
main = do
    putStrLn "Bem-Vindo ao Jogo da Forca!"
    putStrLn "Escolha uma das seguintes opções:"
    putStrLn "1. Regras"
    putStrLn "2. Jogar"
    opcao <- getLine
    if opcao == "1"
        then regras
    else if opcao == "2"
        then jogar
    else putStrLn "Opção invalida"

-- funcoes necessarias: 1. Verifica e Preenche letras 2.

regras :: IO ()
regras = do
    putStrLn "Regras do Jogo:"
    putStrLn "1. Você tem 6 vidas!"
    putStrLn "2. Existe um poder especial. \n Ele sorteará um dos seguintes itens para você:\n uma vida extra (apagará um erro) ou liberará uma letra da palavra."
    putStrLn "\n Para retornar ao menu, digite 1"
    putStrLn "Para começar a jogar, digite 2"
    opcao <- getLine
    if opcao == "1"
        then main
    else if opcao == "2"
        then jogar
    else putStrLn "Opção invalida"

jogar :: IO ()
jogar = do
    putStrLn "Vamos começar!"