Class MainWindow
    ' the code present throughout the project uses for each loops in several places
    ' for justification of this non-nesa control structure, please see the journal entry on the 23/05/2019

    Dim c_display As New Canvas
    Dim tokenDefinitions As IUPACParser.TokenDefinition()
    Dim functionalGroupDefinitions As IUPACParser.FunctionalGroupDefinition()
    Dim examples As XElement

    ''' <summary>
    ''' Called on form load. Sets the window icon, disables exporting the empty canvas and generates tokens and functional group definitions
    ''' Also initialises examples
    ''' </summary>
    ''' <param name="sender">Sent automatically by system</param>
    ''' <param name="e">Sent automatically by system</param>
    ''' <remarks></remarks>
    Private Sub windowLoad(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles MyBase.Loaded
        ' set icon
        Dim iconUri As New Uri(OCVresources.iconPath)
        frm_mainWindow.Icon = System.Windows.Interop.Imaging.CreateBitmapSourceFromHBitmap(My.Resources.icon.GetHbitmap(), _
                                                                                           IntPtr.Zero, Int32Rect.Empty, _
                                                                                           BitmapSizeOptions.FromEmptyOptions())

        ' disable export when nothing to export
        mi_export.IsEnabled = False

        ' load examples
        Dim examplesResource As New XDocument()
        examplesResource = XDocument.Load(New System.IO.StringReader(My.Resources.examples))
        examples = examplesResource.Root

        ' generate examples menu items
        For Each example In examples.Elements()
            Dim newMenuItem As New MenuItem
            newMenuItem.Header = example.Attribute("displayName").Value
            newMenuItem.AddHandler(MenuItem.ClickEvent, New RoutedEventHandler(AddressOf handleExampleMenuItemClick))
            mi_examples.Items.Add(newMenuItem)

        Next

        ' generate token and functional group definitions
        Dim tokenResource As New XDocument()
        tokenResource = XDocument.Load(New System.IO.StringReader(My.Resources.tokens))
        tokenDefinitions = IUPACParser.generateTokenDefinitions(tokenResource.Root)

        Dim functionalGroupDefinitionsResource As New XDocument()
        functionalGroupDefinitionsResource = XDocument.Load(New System.IO.StringReader(My.Resources.functionalGroups))
        functionalGroupDefinitions = IUPACParser.generateFunctionalGroupDefinitions(functionalGroupDefinitionsResource.Root)
    End Sub

    ''' <summary>
    ''' Handles all click events from examples. Searches through examples for matching example and parses and displays it
    ''' </summary>
    ''' <param name="sender">Reference to the control which called the subroutine</param>
    ''' <param name="e">Provides more information about the event that caused this subroutine to be called</param>
    ''' <remarks></remarks>
    Sub handleExampleMenuItemClick(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs)
        For Each example In examples.Elements()
            If example.Attribute("displayName").Value = CType(sender, MenuItem).Header Then
                textBox_userInput.FontStyle = FontStyles.Normal
                textBox_userInput.Foreground = New SolidColorBrush(Colors.Black)
                textBox_userInput.Text = example.Value()
                parseAndDisplay(textBox_userInput.Text)
            End If
        Next
    End Sub

    ''' <summary>
    ''' Given an name to verify, return false if the name is less than 5 characters.
    ''' This is as no organic name is less than 5 characters.
    ''' </summary>
    ''' <param name="name">The name to validate</param>
    ''' <returns>A boolean which is set to True if the name is valid and False if it is not</returns>
    ''' <remarks></remarks>
    Private Function nameLengthValidation(ByVal name As String) As Boolean
        If name.Length < 5 Then
            Return False
        End If
        ' when the function returns it exits the function
        ' so if execution reaches here it means the name is not less than 5, making it valid
        Return True
    End Function

    ''' <summary>
    ''' Given an organicName, uses the functions and subroutines present in IUPACParser and IUPACRenderer to display either an error for an incorrect name
    ''' or a rendered picture of the compound
    ''' </summary>
    ''' <param name="organicName">A string containing the name to be rendered</param>
    ''' <remarks></remarks>
    Private Sub parseAndDisplay(ByVal organicName As String)
        ' wipe canvas on new input
        c_display.Children.Clear()
        mi_export.IsEnabled = False

        If Not nameLengthValidation(organicName) Then
            ' check if name length within bounds
            tb_error.Text = "the name """ & organicName & """ is not valid" & Environment.NewLine & _
                            organicName & Environment.NewLine & _
                            "^" & Environment.NewLine & _
                            "too short"
            ' reset canvas size so vertical scrollbar doesn't show
            c_containerDisplay.Height = 10
            c_containerDisplay.Width = 10
        Else
            ' tokenise example name
            Dim tokens As Token()
            tokens = IUPACParser.generateTokens(organicName, tokenDefinitions)

            ' display error if there is one
            If tokens(0).type = "ERROR" Then
                tb_error.Text = tokens(0).value
                ' reset canvas size so vertical scrollbar doesn't show
                c_containerDisplay.Height = 10
                c_containerDisplay.Width = 10
            Else
                ' convert tokens to AST
                Dim AST As IUPACParser.ASTRoot
                AST = IUPACParser.generateAST(organicName, tokens, tokenDefinitions, functionalGroupDefinitions)

                ' display error if there is one, otherwise render
                If AST.compoundTree.isError Then
                    tb_error.Text = AST.compoundTree.errorMessage
                    ' reset canvas size so vertical scrollbar doesn't show
                    c_containerDisplay.Height = 10
                    c_containerDisplay.Width = 10
                Else
                    ' remove previously displayed AST
                    If c_containerDisplay.Children.Contains(c_display) Then
                        c_containerDisplay.Children.Remove(c_display)
                    End If

                    ' remove error message
                    tb_error.Text = ""

                    ' display rendered AST
                    c_display = IUPACRenderer.renderAST(AST.compoundTree, _
                                                        functionalGroupDefinitions,
                                                        OCVresources.scale, _
                                                        OCVresources.alkaneSpacing, OCVresources.alkaneRise, _
                                                        OCVresources.canvasOffset(0), OCVresources.canvasOffset(1), _
                                                        OCVresources.alkaneStartsRising, OCVresources.alkeEneYneLineXOffsetPercentage, _
                                                        OCVresources.alkEneYneLineYOffsetPercentage, _
                                                        True).canvas

                    c_containerDisplay.Children.Add(c_display)

                    ' enable export
                    mi_export.IsEnabled = True

                    ' set container size to newly rendered canvas size so autoscrolls work
                    c_containerDisplay.Height = c_display.Height
                    c_containerDisplay.Width = c_display.Width
                End If
            End If
        End If
    End Sub

    ''' <summary>
    ''' Given a file path and a name, save the name to the file
    ''' </summary>
    ''' <param name="filePath">A string containing the file path to save to</param>
    ''' <param name="name">The name to be saved to the file</param>
    ''' <remarks></remarks>
    Private Sub saveToFile(ByVal filePath As String, ByVal name As String)
        ' write data to file
        Dim fileWriter As New System.IO.StreamWriter(filePath)
        fileWriter.WriteLine(name)
        ' close file
        fileWriter.Close()
    End Sub

    ''' <summary>
    ''' Given a file path, load the name present in the file
    ''' </summary>
    ''' <param name="filePath">The file path of the file to be read</param>
    ''' <returns>A string containing the name present in the file</returns>
    ''' <remarks></remarks>
    Private Function openFile(ByVal filePath As String) As String
        ' open file (assumes filePath is valid)
        FileSystem.FileOpen(1, filePath, OpenMode.Input)

        ' read data from file
        Dim loadedName As String
        loadedName = FileSystem.LineInput(1)

        ' close file
        FileSystem.FileClose(1)

        Return loadedName
    End Function

    ''' <summary>
    ''' Sets the placeholder text 'Enter IUPAC name' to the user input textbox (textBox_userInput) when it loses focus. Also makes the text italic and gray.
    ''' </summary>
    ''' <param name="sender">Reference to the control which called the subroutine</param>
    ''' <param name="e">Provides more information about the event that caused this subroutine to be called</param>
    ''' <remarks></remarks>
    Private Sub textBox_userInput_setPlaceHolderText(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles textBox_userInput.LostFocus
        If textBox_userInput.Text = "" Then
            textBox_userInput.FontStyle = FontStyles.Italic
            textBox_userInput.Foreground = New SolidColorBrush(ColorConverter.ConvertFromString("#9b9b9b"))
            textBox_userInput.Text = "Enter IUPAC name"
        End If
    End Sub

    ''' <summary>
    ''' Removes the placeholder text and all styling from it for normal use when clicked on by user.
    ''' </summary>
    ''' <param name="sender">Reference to the control which called the subroutine</param>
    ''' <param name="e">Provides more information about the event that caused this subroutine to be called</param>
    ''' <remarks></remarks>
    Private Sub textBox_userInput_removePlaceHolderText(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles textBox_userInput.GotFocus
        If textBox_userInput.Text = "Enter IUPAC name" Then
            textBox_userInput.FontStyle = FontStyles.Normal
            textBox_userInput.Foreground = New SolidColorBrush(Colors.Black)
            textBox_userInput.Text = ""
        End If
    End Sub

    ''' <summary>
    ''' When a name is entered to textBox_userInput and the user presses enter, this subroutine calls parseAndDisplay with the entered name
    ''' </summary>
    ''' <param name="sender">Reference to the control which called the subroutine</param>
    ''' <param name="e">Provides more information about the event that caused this subroutine to be called</param>
    ''' <remarks></remarks>
    Private Sub textBox_userInput_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Input.KeyEventArgs) Handles textBox_userInput.KeyDown
        If e.Key = Key.Enter Then
            parseAndDisplay(textBox_userInput.Text)
        End If
    End Sub

    ''' <summary>
    ''' Called when user clicks on the export menu item. Opens a file dialog and exports the loaded compound as an image into a file location
    ''' of the users choice in either PNG or JPG format.
    ''' </summary>
    ''' <param name="sender">Reference to the control which called the subroutine</param>
    ''' <param name="e">Provides more information about the event that caused this subroutine to be called</param>
    ''' <remarks></remarks>
    Private Sub mi_export_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles mi_export.Click
        Dim chooseLocationDialog As New Microsoft.Win32.SaveFileDialog()
        chooseLocationDialog.Filter = "PNG Files (*.png)|*.png|JPG Files (*.jpg)|*.jpg"
        chooseLocationDialog.Title = "Export"
        Dim location = chooseLocationDialog.ShowDialog()
        If location Then
            If chooseLocationDialog.FilterIndex = 1 Then
                IUPACRenderer.exportCanvasAsImage(c_display, "png", chooseLocationDialog.FileName)
            Else
                IUPACRenderer.exportCanvasAsImage(c_display, "jpg", chooseLocationDialog.FileName)
            End If
        End If
    End Sub

    ''' <summary>
    ''' Called when user clicks on the exit menu item. Exits the application.
    ''' </summary>
    ''' <param name="sender">Reference to the control which called the subroutine</param>
    ''' <param name="e">Provides more information about the event that caused this subroutine to be called</param>
    ''' <remarks></remarks>
    Private Sub mi_exit_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles mi_exit.Click
        frm_mainWindow.Close()
    End Sub

    ''' <summary>
    ''' Called when user clicks on the save menu item. Opens a file dialog and saves the loaded compound into a file of the users choice.
    ''' </summary>
    ''' <param name="sender">Reference to the control which called the subroutine</param>
    ''' <param name="e">Provides more information about the event that caused this subroutine to be called</param>
    ''' <remarks></remarks>
    Private Sub mi_save_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles mi_save.Click
        Dim chooseLocationDialog As New Microsoft.Win32.SaveFileDialog()
        chooseLocationDialog.Filter = "OCV Files (*.ocv)|*.ocv"
        chooseLocationDialog.Title = "Save"
        Dim location = chooseLocationDialog.ShowDialog()
        If location Then
            saveToFile(chooseLocationDialog.FileName, textBox_userInput.Text)
        End If
    End Sub

    ''' <summary>
    ''' Called when the user clicks on the open menu item. Opens a file dialog for the user to choose a file to open and loads the name
    ''' stored in this file
    ''' </summary>
    ''' <param name="sender">Reference to the control which called the subroutine</param>
    ''' <param name="e">Provides more information about the event that caused this subroutine to be called</param>
    ''' <remarks></remarks>
    Private Sub mi_load_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles mi_load.Click
        Dim chooseLocationDialog As New Microsoft.Win32.OpenFileDialog()
        chooseLocationDialog.Filter = "OCV Files (*.ocv)|*.ocv"
        chooseLocationDialog.Title = "Load"
        Dim location = chooseLocationDialog.ShowDialog()
        If location Then
            textBox_userInput.FontStyle = FontStyles.Normal
            textBox_userInput.Foreground = New SolidColorBrush(Colors.Black)
            textBox_userInput.Text = openFile(chooseLocationDialog.FileName)
            parseAndDisplay(textBox_userInput.Text)
        End If
    End Sub
End Class
