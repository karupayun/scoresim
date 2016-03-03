-- Modulo Parser
--   Parsea el scoreboard en formato html, y transforma la información
--     en un formato deseable, para luego ser trabajada. 
--   Simplemente importa un parser para leer un scoreboard con un formato dado.

module Parser where
import ParserKattis as P (teamsParseados, htmlIn)

teamsParseados = P.teamsParseados
htmlIn = P.htmlIn
