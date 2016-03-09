<apply template="base">
  <dfForm action="/new_submit">
    <dfChildErrorList ref="" />

    <dfLabel ref="teamname">Nombre de Equipo: </dfLabel>
    <dfInputText ref="text1" />
    <br>

    <dfLabel ref="problemNumber">Número de Problema: </dfLabel>
    <dfInput ref="int1" type="number" min="1" step="1" pattern="\d+" />
    <br>

    <dfLabel ref="problemNumber">Resultado: </dfLabel> (0 si la submissión es erronea, 1 si es aceptada)
    <dfInput ref="int2" type="number" min="0" step="1" pattern="\d+" />
    <br>

    <dfLabel ref="minute">Total de intentos: </dfLabel>
    <dfInput ref="int3" type="number" min="1" step="1" pattern="\d+" /> (contanto el primer aceptado, si es el caso)
    <br>

    <dfLabel ref="minute">Minuto de la última submission: </dfLabel>
    <dfInput ref="int4" type="number" min="0" step="1" pattern="\d+" />
    <br>

    <dfInputSubmit value="Submit" />

    <p>Si quiere volver al menú central, ingrese aquí: <a href="/">Return</a></p>

  </dfForm>
</apply>

