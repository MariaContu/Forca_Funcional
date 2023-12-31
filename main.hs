import System.IO
import System.Random (randomRIO)
import Data.Char (isAlpha)
import Control.Monad (when)

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
    else if opcao == "f"
        then putStrLn "Respect was paid, thanks for playing"
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
    chute <- verificaChute [] [] palavra
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
        then do
            hClose handle
            return (reverse acc)
        else do
            line <- hGetLine handle
            readWords handle (line : acc)

sortearNovaPalavra :: String -> IO String
sortearNovaPalavra palavraAtual = do
    chanceSorteio <- randomRIO (0, 1) :: IO Float
    if chanceSorteio <= 0.3
        then do
            putStrLn "Sorteando uma nova palavra..."
            handle <- openFile "palavras.txt" ReadMode
            palavras <- readWords handle []
            hClose handle
            let palavrasRestantes = filter (/= palavraAtual) palavras
            indice <- randomRIO (0, length palavrasRestantes - 1)
            return (palavrasRestantes !! indice)
        else return palavraAtual

rodada :: [Char] -> [Char] -> String -> String -> (String, [Char]) -> IO ()
rodada letrasUsadas letrasIncorretas palavraOculta palavraEscolhida chute = do
    let novasLetrasIncorretas = snd chute
    let tentativasRestantes = 6 - length novasLetrasIncorretas
    let novaPalavraOculta = atualizaPalavraOculta palavraOculta palavraEscolhida (fst chute)
    if '_' `elem` novaPalavraOculta && countUnderscores novaPalavraOculta == 1
        then do
            chanceSorteio <- randomRIO (0, 1) :: IO Float
            if chanceSorteio <= 0.3
                then do
                    novaPalavra <- sortearNovaPalavra palavraEscolhida
                    if novaPalavra == palavraEscolhida
                        then finalizaGame 1 novaPalavra
                        else do
                            let novaPalavraOculta' = atualizaPalavraOculta (replicate (length novaPalavra) '_') novaPalavra (fst chute)
                            rodada letrasUsadas letrasIncorretas novaPalavraOculta' novaPalavra chute
                else continueRodada tentativasRestantes novasLetrasIncorretas novaPalavraOculta
        else continueRodada tentativasRestantes novasLetrasIncorretas novaPalavraOculta
    where
    continueRodada 0 novasLetrasIncorretas novaPalavraOculta = do
        putStrLn $ desenhaBoneco 0
        finalizaGame 0 palavraEscolhida
    continueRodada tentativasRestantes novasLetrasIncorretas novaPalavraOculta = do
        putStrLn "-----------------------------------"
        putStrLn $ "Letras usadas: " ++ reverse letrasUsadas ++ "\n"
        putStrLn $ "Letras incorretas: " ++ reverse novasLetrasIncorretas ++ "\n"
        putStrLn $ desenhaBoneco tentativasRestantes
        putStrLn $ "Palavra oculta: " ++ novaPalavraOculta
        novoChute <- verificaChute letrasUsadas novasLetrasIncorretas palavraEscolhida
        let novasLetrasUsadas = fst novoChute
            novasLetrasIncorretas = snd novoChute
        rodada novasLetrasUsadas novasLetrasIncorretas novaPalavraOculta palavraEscolhida novoChute
        
countUnderscores :: String -> Int
countUnderscores = length . filter (== '_')
        
atualizaPalavraOculta :: String -> String -> String -> String
atualizaPalavraOculta palavraOculta palavraEscolhida chute =
    [if c `elem` chute then c else o | (c, o) <- zip palavraEscolhida palavraOculta]

desenhaBoneco :: Int -> String
desenhaBoneco tentativasRestantes =
    case tentativasRestantes of
        6 -> "   ________\n   |    |\n   |\n   |\n   |\n___|___"
        5 -> "   ________\n   |    |\n   |    O\n   |\n   |\n___|___"
        4 -> "   ________\n   |    |\n   |    O\n   |    |\n   |\n___|___"
        3 -> "   ________\n   |    |\n   |    O\n   |   /|\n   |\n___|___"
        2 -> "   ________\n   |    |\n   |    O\n   |   /|\\\n   |\n___|___"
        1 -> "   ________\n   |    |\n   |    O\n   |   /|\\\n   |   /\n___|___"
        0 -> "   ________\n   |    |\n   |    O\n   |   /|\\\n   |   / \\\n___|___"
        _ -> ""

finalizaGame :: Int -> String -> IO ()
finalizaGame resultado palavraEscolhida = do
    if resultado == 1
        then do
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
                then putStrLn "Obrigado por jogar!"
            else do
                putStrLn "Opção inválida"
                finalizaGame 1 palavraEscolhida
    else do
        putStrLn "-----------------------------------"
        putStrLn $ "Você morreu \nPressione 'F' para pagar seus respeitos \n =( A palavra era: " ++ palavraEscolhida
        putStrLn "Deseja voltar ao menu? Digite 1"
        putStrLn "Deseja jogar novamente? Digite 2"
        putStrLn "Deseja parar de jogar? Digite 3"
        putStrLn "Deseja prestar respeitos? Digite 'F'"
        opcao <- getLine
        if opcao == "1"
            then main
        else if opcao == "2"
            then jogar
        else if opcao == "3"
            then putStrLn "Obrigado por jogar!"
        else if opcao == "F" || opcao == "f"
            then putStrLn "Seus respeitos foram prestados. \nA alma de seu boneco agradece! ;) \n Obrigado por jogar!\n"
        else do
            putStrLn "Opção inválida"
            finalizaGame 0 palavraEscolhida

verificaChute :: String -> [Char] -> String -> IO (String, [Char])
verificaChute letrasUsadas letrasIncorretas palavraEscolhida = do
    putStrLn "Chute uma letra, ou '1' para ativar o Poder Especial: "
    chute <- getLine
    if chute == "1"
        then do
            letrasIncorretas' <- poderEspecial letrasIncorretas
            pure (letrasUsadas, letrasIncorretas')
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
                            pure (letrasUsadas, head chute : letrasIncorretas)
                        else do
                            putStrLn "Letra correta!"
                            pure (head chute : letrasUsadas, letrasIncorretas)

poderEspecial :: [Char] -> IO [Char]
poderEspecial letrasIncorretas = do
    let newIncorretas = if null letrasIncorretas then letrasIncorretas else init letrasIncorretas
    when (newIncorretas /= letrasIncorretas) $ putStrLn "Poder especial usado! Um erro foi removido."
    pure newIncorretas
