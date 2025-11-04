# Makefile para o projeto de Passeio do Cavalo (LP)

# Define o compilador Haskell
GHC = ghc

# Flags de compilação:
# -O2 = Otimização nível 2 (importante para busca!)
# --make = Compila apenas o que for necessário
# -o = Nome do arquivo executável
GHCFLAGS = -O2 --make -o

# Nome do executável
TARGET = passeio

# Arquivo fonte principal
SOURCE = Passeio.hs

# Regra padrão (o que é executado ao digitar 'make')
all: $(TARGET)

# Regra para construir o executável
$(TARGET): $(SOURCE)
	$(GHC) $(GHCFLAGS) $(TARGET) $(SOURCE)

# Regra para limpar os arquivos gerados pela compilação
clean:
	rm -f $(TARGET)
	rm -f *.o
	rm -f *.hi

# Regra para executar o programa com um arquivo de entrada padrão
run: all
	./$(TARGET) < entrada.txt