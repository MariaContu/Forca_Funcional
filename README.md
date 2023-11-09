# Forca_Funcional
Jogo da Forca desenvolvido em Haskell para trabalho final da cadeira de programação funcional

## Lista de Checks

✅ Leitura de arquivo .txt

    Ler arquivo de texto para selecionar uma palavra entre as presentes no arquivo.


✅ Forca Funcional

    Sistema padrão de descobrir palavras da forca. Deve receber letra por letra até que preencha a palavra.


⬜ Sistema de Vidas

    Agora, o jogador tem um total de 6 "vidas", ou seja, chutes. 
    Se errar 6 letras, o jogador perde o jogo e é questionado se deseja jogar novamente ou finalizar o programa.


⬜ Bonequinho em ASCII

    Representação das vidas do jogador como um boneco. 
    A ordem de aparição deve ser: cabeça (1 erro), corpo (2 erros), perna esquerda (3 erros), perna direita (4 erros), braço esquerdo (5 erros) e braço direito (6 erros). Ao completar o boneco, o jogador perde o jogo.


⬜ Sistema de sortear nova palavra (30%)

    Faltando uma letra para completar a palavra, o sistema decide aleatoriamente se será sorteada uma nova palavra. 
    Se acontecer, as letras que já estão na palavra anterior devem ser testadas e guardadas em um array de "letras usadas", que deve ser zerada ao trocar de palavra.

    Além disso, a quantidade de erros continua como anteriormente, visando deixar o jogo mais complicado.


⬜ Poder especial

    Aplicar uma opção de poder especial, que sorteia uma das seguintes ajudas

        - Vida extra (apaga um dos erros)
        - Libera uma letra da palavra