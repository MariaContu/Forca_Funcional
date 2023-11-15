import System.IO
import System.Random (randomRIO)
import Data.Char (isAlpha)

main :: IO ()
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
        putStrLn "Opção inválida =("
        main

regras :: IO ()
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
        putStrLn "Opção inválida"
        regras

jogar :: IO ()
jogar = do
    putStrLn "Vamos começar!"
    palavra <- escolhePalavra
    let tamanhoPalavra = length palavra
        palavraOculta = replicate tamanhoPalavra '_'
    putStrLn palavraOculta
    chute <- verificaChute [] [] palavra -- Adicionando a palavra escolhida como parâmetro
    let letrasUsadas = fst chute
        letrasIncorretas = snd chute
    rodada letrasUsadas letrasIncorretas palavraOculta palavra chute
        

escolhePalavra :: IO String
escolhePalavra = do
    handle <- openFile "palavras.txt" ReadMode
    palavras <- readWords handle []
    hClose handle
    indice <- randomRIO (0, length palavras - 1)
    return (palavras !! indice)

readWords :: Handle -> [String] -> IO [String]
readWords handle acc = do
    eof <- hIsEOF handle
    if eof
        then return (reverse acc)
        else do
            line <- hGetLine handle
            readWords handle (line : acc)

rodada :: [Char] -> [Char] -> String -> String -> (String, [Char]) -> IO ()
rodada letrasUsadas letrasIncorretas palavraOculta palavraEscolhida chute = do
    let novaPalavraOculta = atualizaPalavraOculta palavraOculta palavraEscolhida (fst chute)
        novasLetrasIncorretas = snd chute
    if novaPalavraOculta == palavraEscolhida
        then finalizaGame palavraEscolhida
    else do
        putStrLn "-----------------------------------"
        putStrLn $ "Letras usadas: " ++ letrasUsadas
        putStrLn $ "Letras incorretas: " ++ reverse novasLetrasIncorretas
        putStrLn novaPalavraOculta
        novoChute <- verificaChute letrasUsadas novasLetrasIncorretas palavraEscolhida -- Adicionando a palavraEscolhida como parâmetro
        let novasLetrasUsadas = fst novoChute
            novasLetrasIncorretas = snd novoChute
        rodada novasLetrasUsadas novasLetrasIncorretas novaPalavraOculta palavraEscolhida novoChute
            

atualizaPalavraOculta :: String -> String -> String -> String
atualizaPalavraOculta palavraOculta palavraEscolhida chute =
    [if c `elem` chute then c else o | (c, o) <- zip palavraEscolhida palavraOculta]

finalizaGame :: String -> IO ()
finalizaGame palavraEscolhida = do
    putStrLn "-----------------------------------"
    putStrLn $ "Você acertou. A palavra era: " ++ palavraEscolhida
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
        putStrLn "Opção inválida"
        finalizaGame palavraEscolhida

verificaChute :: String -> [Char] -> String -> IO (String, [Char])
verificaChute letrasUsadas letrasIncorretas palavraEscolhida = do
    putStrLn "Chute uma letra, ou '1' para ativar o Poder Especial: "
    chute <- getLine
    if chute == "1"
        then do
            poderEspecial
            verificaChute letrasUsadas letrasIncorretas palavraEscolhida
    else if null chute || not (isAlpha (head chute))
        then do
            putStrLn "Insira um valor válido."
            verificaChute letrasUsadas letrasIncorretas palavraEscolhida
    else if length chute /= 1
        then do
            putStrLn "Insira apenas uma letra!"
            verificaChute letrasUsadas letrasIncorretas palavraEscolhida
    else if head chute `elem` letrasUsadas || head chute `elem` letrasIncorretas
        then do
            putStrLn "Essa letra já foi enviada. Escolha outra."
            verificaChute letrasUsadas letrasIncorretas palavraEscolhida
    else if head chute `notElem` palavraEscolhida
        then do
            putStrLn "Letra incorreta!"
            return (letrasUsadas, head chute : letrasIncorretas)
    else do
        putStrLn "Letra correta!"
        return (head chute : letrasUsadas, letrasIncorretas)
        

poderEspecial :: IO ()
poderEspecial = do
    putStrLn "Poder especial usado!"
