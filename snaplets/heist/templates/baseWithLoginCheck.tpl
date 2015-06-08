<apply template="base">
  <ifLoggedIn>
      <hr />
      <apply template="_mainMenu"/>
      <hr />
      <apply-content/>
      <hr />
      Logged in as '<loggedInUser/>' (<a href="/logout">Logout</a>)
  </ifLoggedIn>


  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>
</apply>
