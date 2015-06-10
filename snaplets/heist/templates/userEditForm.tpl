<apply template="baseWithLoginCheck">
	<h2>Settings:</h2>
	<dfForm action="${postAction}">	
		<apply template="_userEditForm" />
		<br>
		<dfInputSubmit value="Ok" />
	</dfForm>

	<h2>Blogs:</h2>
	<blogs/>
</apply>
