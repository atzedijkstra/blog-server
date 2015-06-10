<apply template="base">
  <ifLoggedIn>
      <hr />
		<table border="0">
		<tr> <td><apply template="_mainMenu"/></td>
			 <td>Logged in as '<loggedInUser/>' (<a href="/logout">Logout</a>)</td>
		</tr>
		</table>
      <hr />
      <apply-content/>
      <hr />
  </ifLoggedIn>

  <ifLoggedOut>
      <hr />
      <a href="/login">Login</a> or <a href="/newUser">Create a new user</a>
      <hr />
      <apply-content/>
      <hr />
  </ifLoggedOut>
</apply>
