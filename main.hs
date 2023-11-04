import System.IO
import System.Random

main :: IO () --funcao inicial
main = do
    putStrLn "-----------------------------------"
    putStrLn "Bem-Vindo ao Jogo da Forca!"
    putStrLn "Escolha uma das seguintes opções:"
    putStrLn "1. Regras"
    putStrLn "2. Jogar"
    putStrLn "-----------------------------------"
    opcao <- getLine
    if opcao == "1"
        then regras
    else if opcao == "2"
        then jogar
    else do
        putStrLn "Opção invalida =("
        main


regras :: IO () --mostra regras do jogo
regras = do
    putStrLn "-----------------------------------"
    putStrLn "Regras do Jogo:"
    putStrLn "1. Você tem 6 vidas!"
    putStrLn "2. Existe um poder especial. \n   Ele sorteará um dos seguintes itens para você:\n   uma vida extra (apagará um erro) ou liberará uma letra da palavra."
    putStrLn "3. Ao faltar uma letra para você completar a palavra, há uma chance de\n   30% de você receber uma palavra nova.\n   Se isso acontecer, as letras corretas anteriores serão testadas\n   e você continuará com a mesma vida."
    putStrLn "\nPara retornar ao menu, digite 1"
    putStrLn "Para começar a jogar, digite 2"
    putStrLn "-----------------------------------"
    opcao <- getLine
    if opcao == "1"
        then main
    else if opcao == "2"
        then jogar
    else do
        putStrLn "Opção invalida"
        regras

jogar :: IO () --funcao principal do jogo
jogar = do
    putStrLn "Vamos começar!"
    palavra <- escolhePalavra
    putStrLn $ "Palavra escolhida: "++palavra --para testar se sorteia uma palavra do arquivo 



escolhePalavra :: IO String --funcao que sorteia a palavra
escolhePalavra = do
    handle <- openFile "palavras.txt" ReadMode
    palavras <- readWords handle []
    hClose handle
    indice <- randomRIO (0,length palavras -1)
    return (palavras !! indice)

readWords :: Handle -> [String] -> IO [String] --funcao q le as palavras
readWords handle acc = do
    -- Tente ler uma linha do arquivo
    eof <- hIsEOF handle
    if eof
        then return (reverse acc) -- Inverta a lista para preservar a ordem original
        else do
            line <- hGetLine handle
            readWords handle (line : acc)