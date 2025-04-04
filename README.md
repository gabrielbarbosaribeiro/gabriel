# Introdução à Ciência dos Dados - 2025/1

Este repositório contém materiais e projetos desenvolvidos para
a disciplina de Introdução à Ciência dos Dados do Bacharelado
em Administração do IFMG - Campus Formiga, cursada no primeiro
semestre de 2025.


## O que estou aprendendo nesta disciplina?

Nesta disciplina, estou aprendendo fundamentos de Ciência de Dados.
Isso inclui:

- Como aplicar a metodologia CRISP-DM em projetos de análise de dados
- Como usar o IDE RStudio
- Como criar relatórios com o sistema Quarto
- Como organizar projetos de análise de dados
- Como trabalhar com o Git e GitHub
- Como usar a linguagem R para analisar dados


## Estrutura do projeto (Organização das Pastas)

- **dados/**
  - **brutos/**: Arquivos de dados originais
  - **limpos/**: Dados organizados após processamento
 
- **relatorios/**
  - **01_relatorio/**: relatório quarto no formato html
  - **02_relatorio/**: relatório quarto no formato pdf
  - **03_relatorio/**: relatório quarto no formato docx


## Atividades Avaliativas

### Avaliação 1

- **Tema:** Fundamentos de R e Manipulação de Dados
- **Data de entrega:** [Data]
- **Status:** [Não iniciado / Em andamento / Concluído]
- **O que aprendi:** [Preencher durante ou após a conclusão]


## Anotações e Dicas Importantes

[Aqui você pode adicionar lembretes, dicas ou comandos úteis
que aprendeu durante as aulas.]

- Anotação ou dica 1

- Anotação ou dica 2



# Glossário de Termos

Este glossário apresenta uma **versão inicial** dos termos relacionados
às ferramentas, tecnologias e métodos que utilizaremos na disciplina de
Introdução à Ciência de Dados: RStudio, R, Quarto, Markdown,
Git e GitHub, Fundamentos de Probabilidade e Estatística.

> **IMPORTANTE**: Este é apenas um ponto de partida. Você deve
assumir o protagonismo na ampliação e manutenção deste glossário,
adicionando novos termos e conceitos à medida que avança no seu
aprendizado. Anote definições de conceitos que encontrar durante as
aulas e leituras, reescreva explicações com suas próprias palavras e
personalize este recurso para que ele realmente apoie seu
desenvolvimento na disciplina.

O glossário que você construir será uma ferramenta valiosa não apenas
durante este semestre, mas também em seus futuros estudos e atividades
profissionais. Consulte-o e, principalmente, enriqueça-o sempre que
encontrar termos ou conceitos importantes.



## A

**Ambiente de Desenvolvimento Integrado (IDE)**: Software que combina
editor de código ou texto, console, gerenciamento de arquivos e outras
ferramentas para facilitar o desenvolvimento de software.



## C

**Cabeçalho YAML**: Seção no início de um documento Quarto, delimitada
por três traços (---), onde são definidos metadados e opções de formatação
para o documento.

**Células de Código** (*Code Chunks*): Blocos de código em um arquivo
quarto que podem ser executados, gerando resultados que são incluídos
no documento final.

**Chave-valor** (*key-value*): Formato de dados usado em YAML que
associa um nome de propriedade (chave) a um conteúdo ou configuração
específica (valor).

**Clone**: Cópia completa de um repositório, incluindo todos os arquivos,
histórico e branches, para o computador local.

**Código Inline**: Código R incorporado diretamente no texto de um
documento Quarto, indicado pela sintaxe `` `r ` ``, que é executado
durante a renderização.

**Commit**: "Fotografia" do estado do projeto em um determinado
momento feita pelo Git, com uma mensagem descritiva das alterações
realizadas.

**Conflito**: Situação onde o Git não consegue mesclar automaticamente
alterações de diferentes fontes porque ambas modificaram a mesma parte
de um arquivo.

**Console**: Interface de linha de comando onde os códigos R são
executados interativamente e os resultados são mostrados imediatamente.



## E

**echo**: Opção de célula de código que controla se o código fonte
é exibido (true) ou ocultado (false) no documento quarto final.

**Editor de texto/código**: Componente de um IDE onde você escreve e
edita o código antes de executá-lo.

**eval**: Opção de célula de código que determina se o código deve
ser executado (true) ou não (false) durante a renderização de
um arquivo quarto.



## F

**fig-cap**: Opção de célula de código que adiciona uma legenda a
uma figura gerada pelo código em um arquivo quarto.

**Formato de Saída**: Tipo de documento final gerado pelo Quarto,
como HTML, PDF, DOCX, apresentações, entre outros.



## G

**Git**: Sistema de controle de versão distribuído que registra
alterações em arquivos ao longo do tempo.

**Git Bash**: Terminal especial instalado com o Git no Windows que
permite executar comandos Git e outros comandos Unix.

**GitHub**: Plataforma online que hospeda repositórios Git e oferece
ferramentas adicionais para colaboração e desenvolvimento de projetos.



## H

**Hash**: Identificador único (como `f7d2e09`) gerado para cada commit,
permitindo referenciar versões específicas dos arquivos.



## I

**Indentação**: Espaçamento consistente utilizado no YAML para indicar
hierarquia e aninhamento de opções, crucial para o funcionamento
correto dos metadados.

**Issue**: Funcionalidade do GitHub para rastrear tarefas, melhorias,
bugs e outras questões relacionadas a um projeto.



## K

**knitr**: Pacote R responsável por executar o código R em documentos
Quarto, transformando (ou renderizando) o arquivo .qmd em um arquivo
markdown intermediário (.md).



## L

**Linguagem de Marcação**: Sistema de anotações inseridas em um
texto para definir como ele deve ser estruturado, formatado ou
apresentado. Ao contrário das linguagens de programação que executam
comandos, as linguagens de marcação utilizam tags ou comandos para
identificar elementos do documento (como títulos, parágrafos,
listas) sem se preocupar com a lógica computacional. Exemplos
incluem HTML (para páginas web), XML (para dados estruturados),
LaTeX (para documentos científicos) e Markdown (usada no Quarto
para formatação simplificada de texto).



## M

**Main**: Branch principal de um repositório Git (anteriormente
chamado de "master").

**Markdown**: Linguagem de marcação leve usada para formatar textos
nos documentos Quarto, permitindo cabeçalhos, listas, negrito,
itálico, links, entre outros elementos.

**Metadados**: Informações sobre o documento (como título, autor,
data, formato) definidas no cabeçalho YAML de um arquivo quarto.



## O

**Origin**: Nome padrão dado ao repositório remoto (normalmente no
GitHub) a partir do qual um repositório local foi clonado.



## P

**Pacote**: Coleção de funções, dados e documentação que estende
as funcionalidades da linguagem R.

**Painel** (*Pane*): Áreas da interface do RStudio que contêm
diferentes ferramentas, como editor, console, ambiente, etc.

**Pandoc**: Ferramenta universal de conversão de documentos que
o Quarto utiliza para transformar arquivos markdown em formatos
finais como HTML, PDF e DOCX.

**Projeto RStudio**: Sistema que organiza arquivos relacionados a
uma análise específica em uma estrutura coerente, facilitando a
organização e reprodutibilidade.

**Pull**: Ação de baixar as alterações do repositório remoto (GitHub)
para o repositório local, atualizando-o.

**Push**: Ação de enviar commits do repositório local para o
repositório remoto (GitHub).

**Publicação Técnica e Científica**: Processo de criar e disseminar
documentos com conteúdo acadêmico ou técnico, seguindo padrões
estabelecidos de formatação e referenciação.



## Q

**QMD**: Extensão de arquivo (.qmd) específica dos documentos Quarto,
que contém texto em Markdown, células de código e metadados YAML.

**Quarto**: Sistema de publicação científica e técnica de código
aberto que permite combinar código, texto narrativo, equações e
visualizações em um único documento. Sucessor do R Markdown.



## R

**R**: Linguagem de programação especializada em análise estatística e
visualização de dados.

**Referência Cruzada**: Recurso que permite fazer referência a elementos
como figuras, tabelas e seções em qualquer parte do documento usando
identificadores únicos.

**Render**: Processo de transformar um arquivo Quarto (.qmd) em seu
formato final (HTML, PDF, etc.), executando código e formatando o
conteúdo conforme as especificações.

**Repositório**: Coleção de arquivos e pastas de um projeto, junto
com o histórico completo de alterações.

**Repositório Local**: Versão do repositório armazenada no seu
computador.

**Repositório Remoto**: Versão do repositório armazenada em um
servidor (como o GitHub).

**Reprodutibilidade**: Princípio que permite que outros pesquisadores
e profissionais possam replicar exatamente os mesmos resultados
utilizando os mesmos dados e códigos. No contexto corporativo, garante
a auditabilidade de análises, facilita a transferência de conhecimento
entre equipes, permite verificação de resultados por diferentes
stakeholders e assegura a continuidade de projetos mesmo com
mudanças de pessoal.



## S

**Stage/Staging Area**: Área intermediária onde as alterações são
adicionadas (via `git add`) antes de serem definitivamente salvas
em um commit.



## T

**Terminal**: Interface de linha de comando onde os comandos Git
são executados.

**TOC** (*Table of Contents*): Sumário ou índice automático gerado
pelo Quarto com base na estrutura de títulos e subtítulos do documento.

**Typst**: Sistema moderno de tipografia utilizado pelo Quarto como
alternativa ao LaTeX para produzir documentos PDF com alta qualidade
tipográfica.



## W

**warning**: Opção de célula de código que controla se os avisos
gerados durante a execução do código são exibidos (true) ou ocultados
(false) no documento quarto final.

**Working Directory**: Diretório local onde os arquivos do projeto
estão sendo editados ativamente.



## Y

**YAML** (*Yet Another Markup Language*): Linguagem de serialização
de dados legível por humanos usada para os metadados de documentos
Quarto, caracterizada pelo formato de pares chave-valor e indentação
significativa.