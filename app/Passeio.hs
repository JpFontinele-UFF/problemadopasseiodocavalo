-- Data.Set para um controle eficiente das casas visitadas. uma árvore binária balanceada
-- Control.Monad para a função 'msum', que encontra a primeira solução válida (o primeiro 'Just') de uma lista.
import qualified Data.Set as Set
import Control.Monad (msum)

-- Definição de tipos 
type Position = (Int, Int)    -- (Linha, Coluna)
type BoardDims = (Int, Int)   -- (TotalLinhas, TotalColunas)
type Path = [Position]      -- O caminho percorrido
type VisitedSet = Set.Set Position -- Casas já visitadas

-- Lista de todos os 8 movimentos relativos do cavalo
knightMoves :: [Position]
knightMoves = [(1, 2), (1, -2), (-1, 2), (-1, -2),
               (2, 1), (2, -1), (-2, 1), (-2, -1)]

-- Verifica se uma posição está dentro dos limites do tabuleiro
isOnBoard :: BoardDims -> Position -> Bool
isOnBoard (rows, cols) (r, c) =
    r >= 1 && r <= rows && c >= 1 && c <= cols

-- Dado um tabuleiro, um conjunto de casas visitadas e uma posição,
-- retorna a lista de próximos movimentos válidos.
validMoves :: BoardDims -> VisitedSet -> Position -> [Position]
validMoves dims visited (r, c) =
    let 
        -- Calcula todas as 8 posições de destino
        potentialMoves = map (\(kr, kc) -> (r + kr, c + kc)) knightMoves
    in
        -- Filtra apenas as que estão no tabuleiro E não foram visitadas
        filter (\pMoves -> isOnBoard dims pMoves && not (pMoves `Set.member` visited)) potentialMoves

-- A função de backtracking principal.
-- Retorna 'Maybe Path' - 'Just path' se encontrar, 'Nothing' se não.
-- O caminho é construído de forma REVERSA (mais eficiente com 'cons' [:])
knightTourSolver :: Int -> BoardDims -> VisitedSet -> Path -> Maybe Path
knightTourSolver totalSquares dims visited path@(currentPos:_) =

    -- Caso Base: Encontramos uma solução?
    if Set.size visited == totalSquares
    then Just path -- Sim, visitamos todas as casas. Retorna o caminho.
    else
        -- Passo Recursivo: Tentar todos os próximos movimentos válidos
        -- Ex: [Nothing, Nothing, Just [...], Nothing] (Onde o terceiro movimento levou a uma solução, mas os outros falharam).
        let
            nextMoves = validMoves dims visited currentPos
            -- Mapeia a função 'knightTourSolver' para cada próximo movimento
            results = map (\move -> knightTourSolver totalSquares dims (Set.insert move visited) (move : path)) nextMoves
        in
            -- 'msum' pega a primeira solução 'Just' na lista de 'Maybes'.
            -- Se todos forem 'Nothing', retorna 'Nothing'.
            msum results

-- Verifica se duas posições estão a um "pulo do cavalo" de distância
-- (Usado para verificar a condição de "tour aberto")
isKnightMoveAway :: Position -> Position -> Bool
isKnightMoveAway (r1, c1) (r2, c2) =
    (abs (r1 - r2) == 2 && abs (c1 - c2) == 1) ||
    (abs (r1 - r2) == 1 && abs (c1 - c2) == 2)

-- Função principal que "gerencia" a busca
findOpenTour :: BoardDims -> Position -> Maybe Path
findOpenTour dims@(rows, cols) startPos =
    let
        totalSquares = rows * cols
        -- Inicia o knightTourSolver.
        -- retorna o caminho reverso (ex: [posFim, ..., posInicio])
        maybeTour = knightTourSolver totalSquares dims (Set.singleton startPos) [startPos]
    in
    case maybeTour of
        Nothing -> Nothing -- Solver não encontrou *nenhum* caminho
        Just path -> -- Solver encontrou um caminho!
            let
                endPos = head path   -- O último local visitado (head, pois o caminho está reverso)
            in
            -- Verifica a condição do "tour aberto":
            -- A primeira posição (startPos) NÃO PODE ser alcançada
            -- imediatamente após a última (endPos).
            if isKnightMoveAway endPos startPos
            then Nothing -- É um tour FECHADO. Inválido para o problema.
            else Just (reverse path) -- É um tour ABERTO. Sucesso! Retorna o caminho na ordem correta.

-- ---
-- Lógica de Entrada/Saída (I/O)
-- ---

main :: IO ()
main = do
    -- Lê todo o conteúdo da entrada padrão (stdin)
    contents <- getContents
    -- Processa cada linha do arquivo de entrada
    mapM_ processLine (lines contents)

-- Processa uma única linha do arquivo de entrada
processLine :: String -> IO ()
processLine line = do
    let parts = words line
    -- Ignora linhas em branco ou mal formatadas
    if length parts == 4
    then do
        -- Lê os 4 inteiros
        let [r, c, sr, sc] = map read parts
        let dims = (r, c)
        let startPos = (sr, sc)

        -- Validação de entrada básica
        if r <= 0 || c <= 0 || not (isOnBoard dims startPos)
        then putStrLn $ "Entrada invalida (dimensões ou pos. inicial fora do tabuleiro): " ++ line
        else do
            -- Chama o solver
            let result = findOpenTour dims startPos
            -- Imprime o resultado
            printResult dims startPos result
    else
        return () -- Ignora a linha

-- Formata e imprime a saída na tela
printResult :: BoardDims -> Position -> Maybe Path -> IO ()
printResult dims startPos (Just path) = do
    putStrLn $ "CASO: " ++ show dims ++ " inicio: " ++ show startPos
    putStrLn "  SOLUCAO: Caminho encontrado."
    -- Formata o caminho para ser mais legível
    putStr "  CAMINHO: "
    print path
    putStrLn "" -- Linha em branco para separar

printResult dims startPos Nothing = do
    putStrLn $ "CASO: " ++ show dims ++ " inicio: " ++ show startPos
    putStrLn "  SOLUCAO: Nao foi possivel encontrar um caminho aberto."
    putStrLn "" -- Linha em branco para separar