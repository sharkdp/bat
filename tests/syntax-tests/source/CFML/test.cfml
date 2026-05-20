<head> 
<title>Add New Employees</title> 
</head> 
<body> 
<h1>Add New Employees</h1> 
<!--- Action page code for the form at the bottom of this page. ---> 
<!--- Establish parameters for first time through ---> 
<cfparam name="Form.firstname" default=""> 
<cfparam name="Form.lastname" default=""> 
<cfparam name="Form.email" default=""> 
<cfparam name="Form.phone" default=""> 
<cfparam name="Form.department" default=""> 
<!--- If at least the firstname form field is passed, create 
a structure named employee and add values. ---> 
<cfif #Form.firstname# eq ""> 
<p>Please fill out the form.</p> 
<cfelse> 
<cfoutput> 
<cfscript> 
employee=StructNew(); 
employee.firstname = Form.firstname; 
employee.lastname = Form.lastname; 
employee.email = Form.email; 
employee.phone = Form.phone; 
employee.department = Form.department; 
</cfscript> 
<!--- Display results of creating the structure. ---> 
First name is #StructFind(employee, "firstname")#<br> 
Last name is #StructFind(employee, "lastname")#<br> 
EMail is #StructFind(employee, "email")#<br> 
Phone is #StructFind(employee, "phone")#<br> 
Department is #StructFind(employee, "department")#<br> 
</cfoutput> 
<!--- Call the custom tag that adds employees. ---> 
<cf_addemployee empinfo="#employee#"> 
</cfif> 
<!--- The form for adding the new employee information ---> 
<hr> 
<form action="newemployee.cfm" method="Post"> 
First Name:&nbsp; 
<input name="firstname" type="text" hspace="30" maxlength="30"><br> 
Last Name:&nbsp; 
<input name="lastname" type="text" hspace="30" maxlength="30"><br> 
EMail:&nbsp; 
<input name="email" type="text" hspace="30" maxlength="30"><br> 
Phone:&nbsp; 
<input name="phone" type="text" hspace="20" maxlength="20"><br> 
Department:&nbsp; 
<input name="department" type="text" hspace="30" maxlength="30"><br> 
<input type="Submit" value="OK"> 
</form> 
<br> 
</body> 
</html>
