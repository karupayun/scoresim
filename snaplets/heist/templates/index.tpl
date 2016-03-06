<apply template="base">

  <ifLoggedIn>
    <p>
      Bienvenido al simulador!
    </p>

    <p>Congrats!  You're logged in as '<loggedInUser/>'</p>

    <p>Si quiere cargar un nuevo simulacro, ingrese aquí: <a href="/new_contest">New Contest</a></p>    

    <p>Si quiere simular un contest ya cargado, ingrese aquí: <a href="/choose">Choose Contest</a></p>    
    
    <p>Si quiere crear un nuevo equipo, ingrese aquí: <a href="/new_team">New Team</a></p>

    <p>Si quiere borrar un equipo ya existente, ingrese aquí: <a href="/delete_team">Delete Team</a></p>
 
    <p>Si quiere ver la lista de sus equipos activos, ingrese aquí: <a href="/teams">Teams</a></p>

    <p>Si quiere agregar una submission de un equipo, ingrese aquí: <a href="/new_submit">New Submit</a></p>

    <p>Si quiere cambiar el delay del simulacro, ingrese aquí: <a href="/delay">Change Delay</a></p>

    <p>Si quiere iniciar el simulacro, ingrese aquí: <a href="/start">Start</a></p>

    <p>Si quiere ver el scoreboard del simulacro, ingrese aquí: <a href="/score.html">Scoreboard</a></p>

    <p>Si quiere pausar el simulacro, ingrese aquí: <a href="/pause">Pause</a></p>

    <p>Si quiere reanudar el simulacro, ingrese aquí: <a href="/unpause">Unpause</a></p>

    <p>Si quiere parar el simulacro, ingrese aquí: <a href="/stop">Stop</a></p>

    <p><a href="/logout">Logout</a></p>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
