import System.IO
import System.Random
import Data.Char (isAlpha)

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
    --putStrLn $ "Palavra escolhida: "++palavra --para testar se sorteia uma palavra do arquivo 
    let tamanhoPalavra = length palavra
        palavraOculta = replicate tamanhoPalavra '_' --cria a palavra com ____
    putStrLn palavraOculta
    chute <- verificaChute []
    let letrasUsadas = chute
    rodada letrasUsadas palavraOculta palavra chute

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

rodada :: [Char] -> String -> String -> String -> IO ()
rodada letrasUsadas palavraOculta palavraEscolhida chute = do
    let novaPalavraOculta = atualizaPalavraOculta palavraOculta palavraEscolhida chute
    if novaPalavraOculta == palavraEscolhida
        then finalizaGame palavraEscolhida
    else do
        putStrLn "-----------------------------------"
        putStrLn $ "Letras usadas: " ++ letrasUsadas 
        putStrLn novaPalavraOculta
        novoChute <- verificaChute letrasUsadas
        let novasLetrasUsadas = novoChute ++ letrasUsadas
        rodada novasLetrasUsadas novaPalavraOculta palavraEscolhida novoChute

atualizaPalavraOculta :: String -> String -> String -> String
atualizaPalavraOculta palavraOculta palavraEscolhida chute = 
    [if c `elem` chute then c else o | (c, o) <- zip palavraEscolhida palavraOculta]


finalizaGame :: String -> IO()
finalizaGame palavraEscolhida = do
    putStrLn "-----------------------------------"
    putStrLn $ "Você acertou. A palavra era: "++ palavraEscolhida
    putStrLn "Deseja voltar ao menu? Digite 1"
    putStrLn "Deseja jogar novamente? Digite 2"
    putStrLn "Deseja parar de jogar? Digite 3"
    opcao <- getLine
    if opcao == "1"
        then main
    else if opcao == "2"
        then jogar
    else if opcao == "3"
        then putStrLn "Obrigada por jogar!"
    else do
        putStrLn "Opção invalida"
        finalizaGame palavraEscolhida


verificaChute :: String -> IO String
verificaChute letrasUsadas = do
    putStrLn "Chute uma letra, ou '1' para ativar o Poder Especial: "
    chute <- getLine
    if chute == "1"
        then do
            poderEspecial
            verificaChute letrasUsadas
            -- referente ao uso de um poder especial
    else if null chute || not (isAlpha(head chute))
        then do
            putStrLn "Insira um valor válido."
            verificaChute letrasUsadas
    else if head chute `elem` letrasUsadas
        then do
            putStrLn "Essa letra ja foi enviada. Escolha outra."
            verificaChute letrasUsadas
    else
        return chute

poderEspecial :: IO()
poderEspecial = do
    putStrLn "Poder especial usado!"
