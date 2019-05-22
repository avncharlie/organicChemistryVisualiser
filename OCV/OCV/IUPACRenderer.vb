Module IUPACRenderer

    Public Structure PointPath
        Public points As Point()
        Public line As Polyline
    End Structure

    Public Structure TextPoint
        Dim text As String
        Dim color As String
        Dim coords As Point
        Dim textBlock As TextBlock
    End Structure

    Public Structure FunctionalGroupPoints
        Public points As PointPath()
        Public representers As TextPoint()
    End Structure

    Public Structure OrganicCompoundPoints
        Public mainChains As PointPath()
        Public eneYnePoints As PointPath()
        Public simpleSubstituents As FunctionalGroupPoints()
        Public complexSubstituentConnectors As PointPath()
        Public ast As IUPACParser.ASTAlkaneBase
        Public canvas As Canvas
    End Structure

    ''' <summary>
    ''' Helper function used to convert an array of Points into a polyline
    ''' </summary>
    ''' <param name="points">the array of Points to be converted</param>
    ''' <returns>a polyline representing the array of Points passed in</returns>
    ''' <remarks></remarks>
    Private Function generatePolylineFromPoints(ByVal points As Point()) As Polyline
        Dim polyline As Polyline = New Polyline()
        polyline.Stroke = Brushes.Black
        polyline.StrokeThickness = 1

        For index = 0 To points.Length - 1
            polyline.Points.Add(points(index))
        Next

        Return polyline
    End Function

    ''' <summary>
    ''' Helper function used to sort an integer array. Uses the bubble sort algorithm
    ''' </summary>
    ''' <param name="arr">the integer array to be sorted</param>
    ''' <returns>an integer array containing the elements in the given integer array sorted in descending order</returns>
    ''' <remarks></remarks>
    Private Function bubbleSortDescending(ByVal arr As Integer()) As Integer()
        Dim unsortedBoundary = arr.Length - 1
        Dim temporary As Integer
        Dim modifiedArrayThisRun As Boolean

        ' if array has not been modified in a run, that means it is sorted
        While unsortedBoundary > 0 Or modifiedArrayThisRun = True
            modifiedArrayThisRun = False
            ' loop to unsortedBoundary - 1 as condiition in the loop looks ahead item
            For index = 0 To unsortedBoundary - 1
                ' if current element is less than the next element, swap
                If arr(index) > arr(index + 1) Then
                    temporary = arr(index)
                    arr(index) = arr(index + 1)
                    arr(index + 1) = temporary
                    modifiedArrayThisRun = True
                End If
            Next
            ' decrease unsortedBoundary as highest element will have bubbled to top
            unsortedBoundary = unsortedBoundary - 1
        End While

        Return arr
    End Function

    ''' <summary>
    ''' Given an an array of IUPACParser.FunctionalGroupChild structures and a FunctionalGroupPoints structure passed by reference, 
    ''' populate the latter with information from the FunctionGroupChild structure. Used to draw functional group substituents.
    ''' Handles recursive children. Orientation is always assumed to be -
    '''   branch 1: 90 deg (north)
    '''   branch 2: 330 deg (30 deg south from east)
    '''   branch 3: 210 deg (30 deg south from west)
    '''   branch 4: in between branch 1 and 2
    '''   branch 5: in between branch 1 and 3
    '''   branch 6: in between branch 2 and 3
    ''' </summary>
    ''' <param name="children">
    ''' The children to be drawn. Passed byref as the 'branches' attribute of the structure will be modified as the subroutine runs
    ''' </param>
    ''' <param name="output">
    ''' The FunctionalGroupPoints structure to be filled. Passed byref as the main function is recursive, meaning all calls of the
    ''' same function will be editing this same structure
    ''' </param>
    ''' <param name="parentBranches">
    ''' The ConnectingBranches structure to be modified. Passed byref as it can be potentially used and modified by all calls of the
    ''' function
    ''' </param>
    ''' <param name="connectionPoint">The point of contact the current child has with the parent, passed as a Point</param>
    ''' <param name="bondLength">A double containing the bond length of bonds in this structure. Used for drawing</param>
    ''' <param name="branchAngles">An array of doubles containing the angles of branches from 1-6 in radians</param>
    ''' <param name="alkaneRise">
    ''' A double containing the height (y) difference of a low point and high point on a straight chain alkane
    ''' </param>
    ''' <param name="alkEneYneLineYOffsetPercentage">
    ''' A double containing the percentage of the height difference to be used to space out double bonds
    ''' </param>
    ''' <remarks>Not to be called directly - interfaced by the createSimpleSubstituent function</remarks>
    Private Sub createFunctionalGroupChild(ByRef children As IUPACParser.FunctionalGroupChild(),
                                           ByRef output As FunctionalGroupPoints,
                                           ByRef parentBranches As IUPACParser.ConnectingBranches,
                                           ByVal connectionPoint As Point,
                                           ByVal bondLength As Double,
                                           ByVal branchAngles As Double(),
                                           ByVal alkaneRise As Double,
                                           ByVal alkEneYneLineYOffsetPercentage As Double)
        ' loop through all children and create one by one (as each child is created, it takes up a branch)
        For childIndex = 0 To children.Length - 1
            ' --- select branch to be used and mark it as taken on parent and child---
            ' sort branches so last branch will be least preferred branch
            parentBranches.takenBranches = bubbleSortDescending(parentBranches.takenBranches)

            ' branch to be used will be highest possible preferred branch, except for special case methane in which branch to be used = 1
            Dim branch As Integer
            branch = -1
            Dim foundHole As Boolean
            foundHole = False
            ' searching for 'holes' in array -e.g. if 1, 3 are taken branches, branch should be 2
            For branchIndex = 0 To parentBranches.takenBranches.Length - 1
                If branchIndex + 1 < parentBranches.takenBranches(branchIndex) And Not foundHole Then
                    branch = branchIndex + 1
                    foundHole = True
                End If
            Next
            ' if methane with nothing added to it, branch = 1
            If parentBranches.takenBranches.Length = 0 Then
                branch = 1
            End If
            ' no holes found and not methane - branch position will have to be the least preferred position. e.g. if 1, 2, 3 are taken branches, branch should be 4
            If branch = -1 Then
                branch = parentBranches.takenBranches(parentBranches.takenBranches.Length - 1) + 1
            End If

            ' mark branch as taken on parentBranches
            ReDim Preserve parentBranches.takenBranches(parentBranches.takenBranches.Length)
            parentBranches.takenBranches(parentBranches.takenBranches.Length - 1) = branch

            ' mark branch as taken on child (each child can only have one parent, so just set to 1)
            children(childIndex).branches.takenBranches = {1, 2, 3, 6}

            ' --- draw line + representor and recurse ---
            ' draw line (assuming single bond)
            Dim startingPoint As New Point(connectionPoint.X, connectionPoint.Y)
            Dim endingPointReference As New Point(connectionPoint.X, connectionPoint.Y - bondLength)
            Dim endingPoint As New Point(0, 0)

            ' rotate endingPoint 90 deg to right as that is the starting point for branch angles
            endingPoint.X = (endingPointReference.X - startingPoint.X) * Math.Cos(Math.PI / 2) - (endingPointReference.Y - startingPoint.Y) * Math.Sin(Math.PI / 2) + startingPoint.X
            endingPoint.Y = (endingPointReference.X - startingPoint.X) * Math.Sin(Math.PI / 2) + (endingPointReference.Y - startingPoint.Y) * Math.Cos(Math.PI / 2) + startingPoint.Y

            endingPointReference.X = endingPoint.X
            endingPointReference.Y = endingPoint.Y

            ' then rotate endingPoint to branch position
            endingPoint.X = (endingPointReference.X - startingPoint.X) * Math.Cos(-branchAngles(branch - 1)) - (endingPointReference.Y - startingPoint.Y) * Math.Sin(-branchAngles(branch - 1)) + startingPoint.X
            endingPoint.Y = (endingPointReference.X - startingPoint.X) * Math.Sin(-branchAngles(branch - 1)) + (endingPointReference.Y - startingPoint.Y) * Math.Cos(-branchAngles(branch - 1)) + startingPoint.Y

            ' draw representer (single or double bond)
            If children(childIndex).representor <> "" Then
                Dim newTextPoint As TextPoint
                newTextPoint.coords = New Point(endingPoint.X, endingPoint.Y)
                newTextPoint.text = children(childIndex).representor
                newTextPoint.color = children(childIndex).colour
                newTextPoint.textBlock = New TextBlock()
                ReDim Preserve output.representers(output.representers.Length)
                output.representers(output.representers.Length - 1) = newTextPoint
            End If

            If children(childIndex).bondType = 1 Then
                ' if single bond, create point path and finish
                Dim newPointPath As PointPath
                newPointPath.line = New Polyline
                newPointPath.points = {startingPoint, endingPoint}
                ReDim Preserve output.points(output.points.Length)
                output.points(output.points.Length - 1) = newPointPath
            Else
                ' if single bond, generate lines from startingPoint and endingPoint
                Dim angleBack As Double
                Dim angleForward As Double
                ' shifted angles are different depending on what branch the double bond is it
                Select Case branch
                    Case 1
                        angleBack = branchAngles(1)
                        angleForward = branchAngles(2)
                    Case 2
                        angleBack = branchAngles(0)
                        angleForward = branchAngles(2)
                    Case 3
                        angleBack = branchAngles(0)
                        angleForward = branchAngles(1)
                    Case 4
                        angleBack = branchAngles(4)
                        angleForward = branchAngles(5)
                    Case 5
                        angleBack = branchAngles(3)
                        angleForward = branchAngles(5)
                    Case 6
                        angleBack = branchAngles(3)
                        angleForward = branchAngles(4)
                End Select

                ' tedious process of shifting lines to new points
                Dim backStartingPoint As New Point(startingPoint.X, startingPoint.Y)
                Dim backEndingPoint As New Point(endingPoint.X, endingPoint.Y)
                Dim forwardStartingPoint As New Point(startingPoint.X, startingPoint.Y)
                Dim forwardEndingPoint As New Point(endingPoint.X, endingPoint.Y)

                Dim xShift As Double
                xShift = (alkaneRise * alkEneYneLineYOffsetPercentage) / 2
                Dim yShift As Double
                yShift = -2

                backStartingPoint.X = backStartingPoint.X + xShift * Math.Cos(angleBack)
                backStartingPoint.Y = backStartingPoint.Y + yShift * Math.Sin(angleBack)

                backEndingPoint.X = backEndingPoint.X + xShift * Math.Cos(angleBack)
                backEndingPoint.Y = backEndingPoint.Y + yShift * Math.Sin(angleBack)

                forwardStartingPoint.X = forwardStartingPoint.X + xShift * Math.Cos(angleForward)
                forwardStartingPoint.Y = forwardStartingPoint.Y + yShift * Math.Sin(angleForward)

                forwardEndingPoint.X = forwardEndingPoint.X + xShift * Math.Cos(angleForward)
                forwardEndingPoint.Y = forwardEndingPoint.Y + yShift * Math.Sin(angleForward)

                ' put together both lines in 2 PointPaths and add to points
                Dim backPointPath As PointPath
                backPointPath.line = New Polyline
                backPointPath.points = {backStartingPoint, backEndingPoint}
                ReDim Preserve output.points(output.points.Length)
                output.points(output.points.Length - 1) = backPointPath

                Dim forwardPointPath As PointPath
                forwardPointPath.line = New Polyline
                forwardPointPath.points = {forwardStartingPoint, forwardEndingPoint}
                ReDim Preserve output.points(output.points.Length)
                output.points(output.points.Length - 1) = forwardPointPath
            End If

            ' --- recurse ---
            If children(childIndex).children.Length <> 0 Then
                createFunctionalGroupChild(children(childIndex).children, output, children(childIndex).branches, endingPoint, bondLength, branchAngles, alkaneRise, alkEneYneLineYOffsetPercentage)
            End If
        Next
    End Sub

    ''' <summary>
    ''' The children to be drawn. Passed byref as the 'branches' attribute of the structure will be modified as the subroutine ----
    ''' A function to generate a simple substituent as a FunctionalGroupPoint structure. Returns as an array in the case of a
    ''' multiple locant simple substituent.
    ''' </summary>
    ''' <param name="ast">The AST of the whole organic compound</param>
    ''' <param name="simpleSubstituentIndex">The index of the substituent to be created in the ast.simpleSubstituents array</param>
    ''' <param name="functionalGroupDefinitions">An array of functionalGroupDefinitions, used to draw functional groups</param>
    ''' <param name="bondLength">A double containing the bond length of bonds in this structure. Used for drawing</param>
    ''' <param name="mainChainPointPath">The PointPath of the main chain. Used for positioning to attach substituents</param>
    ''' <param name="alkaneStartsRising">A boolean to be set true if the alkane is drawn with the first carbon pair rising</param>
    ''' <returns>
    ''' An array of FunctionalGroupPoints representing the given simpleSubstituents. The array only contain one element unless the
    ''' substituent specified had more than one locant position</returns>
    ''' <remarks></remarks>
    Private Function createSimpleSubstituent(ByVal ast As ASTAlkaneBase,
                                             ByVal simpleSubstituentIndex As Integer,
                                             ByVal functionalGroupDefinitions As FunctionalGroupDefinition(),
                                             ByVal bondLength As Double,
                                             ByVal mainChainPointPath As PointPath,
                                             ByVal alkaneStartsRising As Boolean) As FunctionalGroupPoints()
        Dim substituentPoints As FunctionalGroupPoints()
        substituentPoints = {}

        ' --- get functional group definition for substituent ---
        Dim definition As FunctionalGroupDefinition
        definition.children = {}
        definition.type = ""
        For definitionIndex = 0 To functionalGroupDefinitions.Length - 1
            ' alkenes/ynes are taken care of when drawing the main chain
            If ast.simpleSubstituents(simpleSubstituentIndex).type = functionalGroupDefinitions(definitionIndex).type And Not {"alkene", "alkyne"}.Contains(ast.simpleSubstituents(simpleSubstituentIndex).type) Then
                definition = functionalGroupDefinitions(definitionIndex)
            End If
        Next

        ' loop through multiple locant functional group
        Dim locant As Integer
        For locantIndex = 0 To ast.simpleSubstituents(simpleSubstituentIndex).locants.Length - 1
            locant = CType(ast.simpleSubstituents(simpleSubstituentIndex).locants(locantIndex), Integer)

            ' --- create substituent group ---
            Dim substituentPoint As FunctionalGroupPoints
            substituentPoint.points = {}
            substituentPoint.representers = {}

            createFunctionalGroupChild(definition.children, substituentPoint, ast.branches(locant - 1), mainChainPointPath.points(locant - 1), bondLength, OCVresources.branchAngles, alkaneRise, alkEneYneLineYOffsetPercentage)

            ' --- rotate substituent group as necessary --
            Dim rotationAngle As Double
            rotationAngle = 0
            ' flip valley points (\/)
            If (alkaneStartsRising And locant Mod 2 <> 0) Or (Not alkaneStartsRising And locant Mod 2 = 0) Then
                rotationAngle = Math.PI
            End If
            ' orient cycloalkane points
            If ast.isCyclical Then
                rotationAngle = ((2 * Math.PI) / ast.length) * (locant - 1) - (2 * Math.PI) / (ast.length * 2)
            End If

            ' rotation of substituent
            Dim origin As Point
            origin = mainChainPointPath.points(locant - 1)
            Dim locationCopy As New Point(0, 0)
            ' first rotate points
            For pointPathIndex = 0 To substituentPoint.points.Length - 1
                For pointIndex = 0 To substituentPoint.points(pointPathIndex).points.Length - 1
                    ' rotate each point around specified origin
                    substituentPoint.points(pointPathIndex).points(pointIndex) = rotatePoint(substituentPoint.points(pointPathIndex).points(pointIndex), origin, rotationAngle)
                Next
            Next
            ' next rotate representers
            For representerIndex = 0 To substituentPoint.representers.Length - 1
                substituentPoint.representers(representerIndex).coords = rotatePoint(substituentPoint.representers(representerIndex).coords, origin, rotationAngle)
            Next

            ' add to return array
            ReDim Preserve substituentPoints(substituentPoints.Length)
            substituentPoints(substituentPoints.Length - 1) = substituentPoint
        Next

        ' return array of undrawn or connected substituent points
        Return substituentPoints
    End Function

    ''' <summary>
    ''' Given a point to rotate, an origin point and an angle, returns the point rotated around the origin by the angle
    ''' in a clockwise rotation.
    ''' </summary>
    ''' <param name="point">The point to be rotated</param>
    ''' <param name="origin">Contains the coordinates the point should be rotated around</param>
    ''' <param name="angle">The angle in radians to rotate the point by, as a double</param>
    ''' <returns>The point rotated according to the given specifications</returns>
    ''' <remarks></remarks>
    Private Function rotatePoint(ByVal point As Point, ByVal origin As Point, ByVal angle As Double) As Point
        Dim pointCopy As New Point(point.X, point.Y)
        Dim rotatedPoint As New Point(0, 0)
        rotatedPoint.X = (pointCopy.X - origin.X) * Math.Cos(angle) - (pointCopy.Y - origin.Y) * Math.Sin(angle) + origin.X
        rotatedPoint.Y = (pointCopy.X - origin.X) * Math.Sin(angle) + (pointCopy.Y - origin.Y) * Math.Cos(angle) + origin.Y
        Return rotatedPoint
    End Function

    ''' <summary>
    ''' Given a point to translate and a vector in the form (x vector, y vector) as a point, translate the point by the vector.
    ''' </summary>
    ''' <param name="origPoint">The point to be translated</param>
    ''' <param name="vector">A point in the form (x vector, y vector) to translate origPoint by</param>
    ''' <returns>The point translated according to the given specifications</returns>
    ''' <remarks></remarks>
    Private Function translatePoint(ByVal origPoint As Point, ByVal vector As Point) As Point
        Dim translatedPoint As New Point(origPoint.X + vector.X, origPoint.Y + vector.Y)
        Return translatedPoint
    End Function

    ''' <summary>
    ''' Given an OrganicCompoundPoints, an origin and an angle, rotate the OrganicCompoundPoints around the origin by the angle
    ''' in a clockwise direction.
    ''' </summary>
    ''' <param name="ocPoints">The OrganicCompoundPoints to rotate. Passed byref to save memory</param>
    ''' <param name="origin">The point to rotate ocPoints by</param>
    ''' <param name="angle">The angle in radians to rotate ocPoints by in a clockwise direction</param>
    ''' <remarks></remarks>
    Private Sub rotateOrganicCompountPoints(ByRef ocPoints As OrganicCompoundPoints, ByVal origin As Point, ByVal angle As Double)
        ' rotate main chains
        For mainChainIndex = 0 To ocPoints.mainChains.Length - 1
            For pointPathIndex = 0 To ocPoints.mainChains(mainChainIndex).points.Length - 1
                ocPoints.mainChains(mainChainIndex).points(pointPathIndex) = rotatePoint(ocPoints.mainChains(mainChainIndex).points(pointPathIndex), origin, angle)
            Next
        Next

        ' rotate eneYnePoints
        For eneYneIndex = 0 To ocPoints.eneYnePoints.Length - 1
            For pointPathIndex = 0 To ocPoints.eneYnePoints(eneYneIndex).points.Length - 1
                ocPoints.eneYnePoints(eneYneIndex).points(pointPathIndex) = rotatePoint(ocPoints.eneYnePoints(eneYneIndex).points(pointPathIndex), origin, angle)
            Next
        Next

        ' rotate simpleSubstituents
        For simpleSubstituentsIndex = 0 To ocPoints.simpleSubstituents.Length - 1
            ' points
            For pointPathIndex = 0 To ocPoints.simpleSubstituents(simpleSubstituentsIndex).points.Length - 1
                For pointIndex = 0 To ocPoints.simpleSubstituents(simpleSubstituentsIndex).points(pointPathIndex).points.Length - 1
                    ocPoints.simpleSubstituents(simpleSubstituentsIndex).points(pointPathIndex).points(pointIndex) = rotatePoint(ocPoints.simpleSubstituents(simpleSubstituentsIndex).points(pointPathIndex).points(pointIndex), origin, angle)
                Next
            Next
            ' representors
            For representorIndex = 0 To ocPoints.simpleSubstituents(simpleSubstituentsIndex).representers.Length - 1
                ocPoints.simpleSubstituents(simpleSubstituentsIndex).representers(representorIndex).coords = rotatePoint(ocPoints.simpleSubstituents(simpleSubstituentsIndex).representers(representorIndex).coords, origin, angle)
            Next
        Next

        ' rotate complexSubstituentConnectors
        For complexSubstituentConnectorIndex = 0 To ocPoints.complexSubstituentConnectors.Length - 1
            For pointPathIndex = 0 To ocPoints.complexSubstituentConnectors(complexSubstituentConnectorIndex).points.Length - 1
                ocPoints.complexSubstituentConnectors(complexSubstituentConnectorIndex).points(pointPathIndex) = rotatePoint(ocPoints.complexSubstituentConnectors(complexSubstituentConnectorIndex).points(pointPathIndex), origin, angle)
            Next
        Next
    End Sub

    ''' <summary>
    ''' Given an OrganicCompoundPoints and a vector, translate the OrganicCompoundPoints by the vector.
    ''' </summary>
    ''' <param name="ocPoints">The OrganicCompoundPoints to be translated. Passed byref to save memory</param>
    ''' <param name="vector">The vector in the form (x vector, y vector) to translate ocPoints by</param>
    ''' <remarks></remarks>
    Private Sub translateOrganicCompoundPoints(ByRef ocPoints As OrganicCompoundPoints, ByVal vector As Point)
        ' translate main chains
        For mainChainIndex = 0 To ocPoints.mainChains.Length - 1
            For pointPathIndex = 0 To ocPoints.mainChains(mainChainIndex).points.Length - 1
                ocPoints.mainChains(mainChainIndex).points(pointPathIndex) = translatePoint(ocPoints.mainChains(mainChainIndex).points(pointPathIndex), vector)
            Next
        Next

        ' translate eneYnePoints
        For eneYneIndex = 0 To ocPoints.eneYnePoints.Length - 1
            For pointPathIndex = 0 To ocPoints.eneYnePoints(eneYneIndex).points.Length - 1
                ocPoints.eneYnePoints(eneYneIndex).points(pointPathIndex) = translatePoint(ocPoints.eneYnePoints(eneYneIndex).points(pointPathIndex), vector)
            Next
        Next

        ' translate simpleSubstituents
        For simpleSubstituentsIndex = 0 To ocPoints.simpleSubstituents.Length - 1
            ' points
            For pointPathIndex = 0 To ocPoints.simpleSubstituents(simpleSubstituentsIndex).points.Length - 1
                For pointIndex = 0 To ocPoints.simpleSubstituents(simpleSubstituentsIndex).points(pointPathIndex).points.Length - 1
                    ocPoints.simpleSubstituents(simpleSubstituentsIndex).points(pointPathIndex).points(pointIndex) = translatePoint(ocPoints.simpleSubstituents(simpleSubstituentsIndex).points(pointPathIndex).points(pointIndex), vector)
                Next
            Next
            ' representors
            For representorIndex = 0 To ocPoints.simpleSubstituents(simpleSubstituentsIndex).representers.Length - 1
                ocPoints.simpleSubstituents(simpleSubstituentsIndex).representers(representorIndex).coords = translatePoint(ocPoints.simpleSubstituents(simpleSubstituentsIndex).representers(representorIndex).coords, vector)
            Next
        Next

        ' translate complexSubstituentConnectors
        For complexSubstituentConnectorIndex = 0 To ocPoints.complexSubstituentConnectors.Length - 1
            For pointPathIndex = 0 To ocPoints.complexSubstituentConnectors(complexSubstituentConnectorIndex).points.Length - 1
                ocPoints.complexSubstituentConnectors(complexSubstituentConnectorIndex).points(pointPathIndex) = translatePoint(ocPoints.complexSubstituentConnectors(complexSubstituentConnectorIndex).points(pointPathIndex), vector)
            Next
        Next
    End Sub

    ''' <summary>
    ''' Given an AST, return an OrganicCompoundPoints function containing a populated canvas representing the given AST.
    ''' 1000 line function justification: This function performs one logical task - given a Abstract Syntax Tree, it returns
    ''' an OrganicCompountPoints structure visualising this tree. The component parts of this problem are split into other
    ''' functions and subroutines that are continually used throughout the function - primarily: 
    '''   translateOrganicCompoundPoints, rotateOrganicCompountPoints, translatePoint, rotatePoint.
    ''' All the other functions in this page are also called through this function, and big parts of this problem, such as
    ''' generating simple substituents.
    ''' </summary>
    ''' <param name="ast">The AST to be visualised</param>
    ''' <param name="functionalGroupDefinitions">An array of FunctionalGroupDefinitions</param>
    ''' <param name="scale">
    ''' A double containing the scale of the output. 1 is default, other scales may seem off-kilter. Is optional.
    ''' </param>
    ''' <param name="alkaneSpacing">A double containing the x distance between two points in an alkane chain. Is optional.</param>
    ''' <param name="alkaneRise">A double containing the y height difference between two points in an alkane chain. Is optional.</param>
    ''' <param name="canvasOffsetX">
    ''' A double containing how much x offset from the top of the canvas the generated elements should have. Is optional.
    ''' </param>
    ''' <param name="canvasOffsetY">
    ''' A double containing how much y offset from the top of the canvas the generated elements should have. Is optional.
    ''' </param>
    ''' <param name="alkaneStartsRising">
    ''' A boolean, set to true if the alkane chain starts rising from the first carbon-carbon bond. Is optional.
    ''' </param>
    ''' <param name="alkeEneYneLineXOffsetPercentage">
    ''' A double containing a percentage from 0 to 1. This percentage of the alkaneSpacing value is used to offset the x distance
    ''' between a single bond and a double/triple bond line. Is optional.
    ''' </param>
    ''' <param name="alkEneYneLineYOffsetPercentage">
    ''' A double containing a percentage from 0 to 1. This percentage of the alkaneRise value is used to offset the y distance
    ''' between a single bond and a double/triple bond line. Is optional.
    ''' </param>
    ''' <param name="firstCall">
    ''' A boolean to be set to true if this is the first call of the function. Used to differentiate between the first call and other
    ''' recursive calls.
    ''' </param>
    ''' <returns>
    ''' An OrganicCompoundPoints object, which contains a populated canvas object containing the visualisation of the ast.
    ''' </returns>
    ''' <remarks>The exportCanvasAsImage function can be used to export the canvas as an image.</remarks>
    Public Function renderAST(ByRef ast As IUPACParser.ASTAlkaneBase, _
                              ByVal functionalGroupDefinitions As IUPACParser.FunctionalGroupDefinition(),
                              Optional ByVal scale As Double = 1,
                              Optional ByVal alkaneSpacing As Integer = 30,
                              Optional ByVal alkaneRise As Integer = 17,
                              Optional ByVal canvasOffsetX As Integer = 30,
                              Optional ByVal canvasOffsetY As Integer = 30,
                              Optional ByVal alkaneStartsRising As Boolean = True,
                              Optional ByVal alkeEneYneLineXOffsetPercentage As Double = 2 / 30,
                              Optional ByVal alkEneYneLineYOffsetPercentage As Double = 4 / 17,
                              Optional ByVal firstCall As Boolean = True) As OrganicCompoundPoints
        ' initialise canvas
        Dim display As New Canvas
        display.Background = Brushes.White

        ' initialise organicCompoundPoints for current alkane
        Dim mainChainPointPath As PointPath
        mainChainPointPath.points = {}
        mainChainPointPath.line = New Polyline()

        Dim ocPoints As OrganicCompoundPoints
        ocPoints.mainChains = {mainChainPointPath}
        ocPoints.eneYnePoints = {}
        ocPoints.simpleSubstituents = {}
        ocPoints.complexSubstituentConnectors = {}
        ocPoints.ast = ast

        ' set scale
        alkaneSpacing = alkaneSpacing * scale
        alkaneRise = alkaneRise * scale

        ' generate ene and yne locants
        Dim eneLocants As Integer()
        eneLocants = {}
        Dim yneLocants As Integer()
        yneLocants = {}
        For index = 0 To ast.simpleSubstituents.Length - 1
            If ast.simpleSubstituents(index).type = "alkene" Then
                For locantIndex = 0 To ast.simpleSubstituents(index).locants.Length - 1
                    ReDim Preserve eneLocants(eneLocants.Length)
                    eneLocants(eneLocants.Length - 1) = CType(ast.simpleSubstituents(index).locants(locantIndex), Integer)
                Next
            Else
                If ast.simpleSubstituents(index).type = "alkyne" Then
                    For locantIndex = 0 To ast.simpleSubstituents(index).locants.Length - 1
                        ReDim Preserve yneLocants(yneLocants.Length)
                        yneLocants(yneLocants.Length - 1) = CType(ast.simpleSubstituents(index).locants(locantIndex), Integer)
                    Next
                End If
            End If
        Next

        ' calculate bond length
        Dim bondLength As Double
        bondLength = (alkaneSpacing ^ 2 + alkaneRise ^ 2) ^ (1 / 2)

        ' --- main chain (and generate branches on ast) ---
        Dim isRising As Boolean
        isRising = alkaneStartsRising

        Dim previousPoint As Point = New Point()
        Dim newPoint As Point = New Point(0, 0)

        Dim currentLocant As Integer

        ' straight chains are dealt with differently to cyclical chains
        If Not ast.isCyclical Then
            ' straight chain alkane base

            For index = 0 To ast.length - 1
                ' as the 'locant' of the current point is the index+1 (0 indexing or arrays vs 1 indexing of locants)
                currentLocant = index + 1

                ' generate connecting branches on AST
                Dim branches As IUPACParser.ConnectingBranches
                branches.takenBranches = {}
                Select Case currentLocant
                    Case 1
                        ' first locant
                        If alkaneStartsRising Then
                            branches.takenBranches = {3}
                        Else
                            branches.takenBranches = {2}
                        End If
                    Case ast.length
                        ' last locant
                        If alkaneStartsRising Then
                            If ast.length Mod 2 = 0 Then
                                branches.takenBranches = {3}
                            Else
                                branches.takenBranches = {2}
                            End If
                        Else
                            If ast.length Mod 2 = 0 Then
                                branches.takenBranches = {2}
                            Else
                                branches.takenBranches = {3}
                            End If
                        End If
                    Case Else
                        ' middle locant
                        branches.takenBranches = {2, 3}
                End Select
                ' special case for methane - none taken
                If ast.length = 1 Then
                    branches.takenBranches = {}
                End If
                ' add to byref passed ast and ast stored in ocPoints
                ReDim Preserve ast.branches(ast.branches.Length)
                ast.branches(ast.branches.Length - 1) = branches
                ReDim Preserve ocPoints.ast.branches(ocPoints.ast.branches.Length)
                ocPoints.ast.branches(ocPoints.ast.branches.Length - 1) = branches

                ' if the current locant is an alkyne or an alkene, create polylines for it (dashes above and below points)
                If yneLocants.Contains(currentLocant - 1) Or eneLocants.Contains(currentLocant - 1) Then
                    ' init variables for above and below bars (dim so that it is created every time it needs to be, no linking)
                    Dim eneYneAboveBar As PointPath
                    eneYneAboveBar.points = {Nothing, Nothing}
                    eneYneAboveBar.line = New Polyline()

                    Dim eneYneBelowBar As PointPath
                    eneYneBelowBar.points = {Nothing, Nothing}
                    eneYneBelowBar.line = New Polyline()

                    ' first the above polyline (for both ene and yne)
                    If isRising Then
                        eneYneAboveBar.points(0) = New Point(previousPoint.X + (alkeEneYneLineXOffsetPercentage * alkaneSpacing), previousPoint.Y - (alkEneYneLineYOffsetPercentage * alkaneRise))
                        eneYneAboveBar.points(1) = New Point(newPoint.X + (alkeEneYneLineXOffsetPercentage * alkaneSpacing), newPoint.Y - (alkEneYneLineYOffsetPercentage * alkaneRise))
                    Else
                        eneYneAboveBar.points(0) = New Point(previousPoint.X - (alkeEneYneLineXOffsetPercentage * alkaneSpacing), previousPoint.Y - (alkEneYneLineYOffsetPercentage * alkaneRise))
                        eneYneAboveBar.points(1) = New Point(newPoint.X - (alkeEneYneLineXOffsetPercentage * alkaneSpacing), newPoint.Y - (alkEneYneLineYOffsetPercentage * alkaneRise))
                    End If

                    ' add to eneYnePoints
                    ReDim Preserve ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length)
                    ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length - 1) = eneYneAboveBar

                    ' then the below polyline (only for yne)
                    If yneLocants.Contains(currentLocant - 1) Then
                        If isRising Then
                            eneYneBelowBar.points(0) = New Point(previousPoint.X - (alkeEneYneLineXOffsetPercentage * alkaneSpacing), previousPoint.Y + (alkEneYneLineYOffsetPercentage * alkaneRise))
                            eneYneBelowBar.points(1) = New Point(newPoint.X - (alkeEneYneLineXOffsetPercentage * alkaneSpacing), newPoint.Y + (alkEneYneLineYOffsetPercentage * alkaneRise))
                        Else
                            eneYneBelowBar.points(0) = New Point(previousPoint.X + (alkeEneYneLineXOffsetPercentage * alkaneSpacing), previousPoint.Y + (alkEneYneLineYOffsetPercentage * alkaneRise))
                            eneYneBelowBar.points(1) = New Point(newPoint.X + (alkeEneYneLineXOffsetPercentage * alkaneSpacing), newPoint.Y + (alkEneYneLineYOffsetPercentage * alkaneRise))
                        End If

                        ' add to eneYnePoints
                        ReDim Preserve ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length)
                        ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length - 1) = eneYneBelowBar
                    End If
                End If

                ' if currentLocant is alkyne or previous locant was an alkyne, or if two or more alkenes in a row, set flip isRising so alkane goes straight
                If yneLocants.Contains(currentLocant) Or yneLocants.Contains(currentLocant - 1) Or (eneLocants.Contains(currentLocant) And eneLocants.Contains(currentLocant - 1)) Then
                    isRising = Not isRising
                End If

                ' add current point to points array to be converted to polyline
                ReDim Preserve ocPoints.mainChains(0).points(ocPoints.mainChains(0).points.Length)
                ocPoints.mainChains(0).points(ocPoints.mainChains(0).points.Length - 1) = newPoint

                ' move next point up or down depending on isRising
                previousPoint = newPoint
                newPoint.X = newPoint.X + alkaneSpacing
                If isRising Then
                    newPoint.Y = newPoint.Y - alkaneRise
                Else
                    newPoint.Y = newPoint.Y + alkaneRise
                End If
                isRising = Not isRising
            Next
        Else
            ' cyclical alkane base

            ' will be regular polygon with n sides, can be represented as n congruent iscosceles triangles with tips all at one point
            ' calculate interior (turning) angle
            Dim turnAngle As Double
            turnAngle = (2 * Math.PI) / ast.length

            ' calculate "radius" of polygon (length of equal side in the iscosceles triangle) (cosine rule)
            Dim polygonRadius As Double
            polygonRadius = Math.Sqrt((bondLength ^ 2) / (2 - 2 * Math.Cos(turnAngle)))

            ' to rotate a point (x, y) around the origin (0, 0) 'a' degrees, the new point would be (x * cos(a) - y * sin(a), x * sin(a) + y * cos(a))
            ' use this to create polygon 

            ' calculate starting points (for top side of polygon to be straight, get point straight up and then rotate counter clockwise half of the turning angle)
            Dim polygonPoint As Point = New Point(0, -1 * polygonRadius)
            Dim currentAlkenePoint As Point = New Point(polygonPoint.X, polygonPoint.Y + (alkaneRise * alkEneYneLineYOffsetPercentage))
            polygonPoint = New Point(polygonPoint.X * Math.Cos(turnAngle / 2) + polygonPoint.Y * Math.Sin(turnAngle / 2), polygonPoint.X * Math.Sin(turnAngle / 2) + polygonPoint.Y * Math.Cos(turnAngle / 2))
            currentAlkenePoint = New Point(currentAlkenePoint.X * Math.Cos(turnAngle / 2) + currentAlkenePoint.Y * Math.Sin(turnAngle / 2), currentAlkenePoint.X * Math.Sin(turnAngle / 2) + currentAlkenePoint.Y * Math.Cos(turnAngle / 2))

            ' store starting point to attach to the end
            Dim startingPoint As Point = New Point(polygonPoint.X, polygonPoint.Y)

            currentLocant = 0
            Dim previousAlkenePoint As New Point()
            Dim theta As Double
            Dim debugTheta As Double

            Dim topBottomDifference As Double
            topBottomDifference = currentAlkenePoint.Y - polygonPoint.Y
            For index = 0 To ast.length - 1
                currentLocant = index + 1

                Dim branches As IUPACParser.ConnectingBranches
                branches.takenBranches = {2, 3} ' branches 2, 3 always taken on cyclical alkane
                ReDim Preserve ast.branches(ast.branches.Length)
                ast.branches(ast.branches.Length - 1) = branches

                If eneLocants.Contains(currentLocant - 1) Or yneLocants.Contains(currentLocant - 1) Then
                    ' draw ene line or bottom yne line
                    ReDim Preserve ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length)
                    ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length - 1).points = {
                        New Point(previousAlkenePoint.X, previousAlkenePoint.Y),
                        New Point(currentAlkenePoint.X, currentAlkenePoint.Y)
                    }

                    ' draw top yne line if needed
                    If yneLocants.Contains(currentLocant - 1) Then

                        Dim distance As Double
                        distance = topBottomDifference * 2

                        ReDim Preserve ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length)

                        ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length - 1).points = {
                            New Point(
                                previousAlkenePoint.X + (distance * Math.Cos(theta)),
                                previousAlkenePoint.Y - (distance * Math.Sin(theta))
                            ),
                            New Point(
                                currentAlkenePoint.X + (distance * Math.Cos(theta)),
                                currentAlkenePoint.Y - (distance * Math.Sin(theta))
                            )
                        }
                    End If
                End If

                ' copy of polygonPoint that is redeclared every loop so points aren't linked
                Dim pointToAdd As Point = New Point(polygonPoint.X, polygonPoint.Y)

                ' rotate polygonPoint
                ReDim Preserve ocPoints.mainChains(0).points(ocPoints.mainChains(0).points.Length)
                ocPoints.mainChains(0).points(ocPoints.mainChains(0).points.Length - 1) = pointToAdd
                polygonPoint = New Point(polygonPoint.X * Math.Cos(turnAngle) - polygonPoint.Y * Math.Sin(turnAngle), polygonPoint.X * Math.Sin(turnAngle) + polygonPoint.Y * Math.Cos(turnAngle))

                ' store currentAlkenePoint and rotate
                previousAlkenePoint = New Point(currentAlkenePoint.X, currentAlkenePoint.Y)
                currentAlkenePoint = New Point(currentAlkenePoint.X * Math.Cos(turnAngle) - currentAlkenePoint.Y * Math.Sin(turnAngle), currentAlkenePoint.X * Math.Sin(turnAngle) + currentAlkenePoint.Y * Math.Cos(turnAngle))

                ' calculate angle vars for top yne bar 
                theta = (Math.PI / 2) - index * ((2 * Math.PI) / ast.length)
                debugTheta = (1 / 2) - index * ((2 * 1) / ast.length)

            Next

            ' check if there is a locant connecting first to last and if there is draw it
            If eneLocants.Contains(currentLocant) Or yneLocants.Contains(currentLocant) Then
                ' draw ene line or bottom yne line
                ReDim Preserve ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length)
                ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length - 1).points = {
                    New Point(previousAlkenePoint.X, previousAlkenePoint.Y),
                    New Point(currentAlkenePoint.X, currentAlkenePoint.Y)
                }

                ' draw top yne line if needed
                If yneLocants.Contains(currentLocant) Then

                    Dim distance As Double
                    distance = topBottomDifference * 2

                    ReDim Preserve ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length)

                    ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length - 1).points = {
                        New Point(
                            previousAlkenePoint.X + (distance * Math.Cos(theta)),
                            previousAlkenePoint.Y - (distance * Math.Sin(theta))
                        ),
                        New Point(
                            currentAlkenePoint.X + (distance * Math.Cos(theta)),
                            currentAlkenePoint.Y - (distance * Math.Sin(theta))
                        )
                    }
                End If
            End If

            ' final point is starting point
            ReDim Preserve ocPoints.mainChains(0).points(ocPoints.mainChains(0).points.Length)
            ocPoints.mainChains(0).points(ocPoints.mainChains(0).points.Length - 1) = startingPoint
        End If

        ' --- complex substituents ---
        ' multi locant complex substituents have to be split up as they share the same connectingBranches object, leading toproblems while drawing
        Dim complexSubstituentIndex = 0
        While complexSubstituentIndex < ast.complexSubstituents.Length
            If ast.complexSubstituents(complexSubstituentIndex).locants.Length > 1 Then
                For locantIndex = 1 To ast.complexSubstituents(complexSubstituentIndex).locants.Length - 1
                    ' copy complex substituent to new locant
                    ReDim Preserve ast.complexSubstituents(ast.complexSubstituents.Length)
                    ast.complexSubstituents(ast.complexSubstituents.Length - 1) = IUPACParser.copyAST(ast.complexSubstituents(complexSubstituentIndex))
                    ' change locant to specific other locant
                    ast.complexSubstituents(ast.complexSubstituents.Length - 1).locants = {ast.complexSubstituents(complexSubstituentIndex).locants(locantIndex)}
                Next

                ' rewrite locants of first complex substituent to just the first locant
                ast.complexSubstituents(complexSubstituentIndex).locants = {ast.complexSubstituents(complexSubstituentIndex).locants(0)}
            End If
            complexSubstituentIndex = complexSubstituentIndex + 1
        End While

        For complexSubstituentIndex = 0 To ast.complexSubstituents.Length - 1
            Dim locant As Integer
            Dim rotation As Double
            Dim cyclicRotation As Double
            Dim translationVector As Point
            For locantIndex = 0 To ast.complexSubstituents(complexSubstituentIndex).locants.Length - 1
                ' get new substituent
                ' has to be declared inside loop otherwise multi locant complex substituents will get linked
                Dim newComplexPoints As OrganicCompoundPoints
                newComplexPoints = renderAST(ast.complexSubstituents(complexSubstituentIndex),
                                             functionalGroupDefinitions,
                                             scale,
                                             alkaneSpacing,
                                             alkaneRise,
                                             canvasOffsetX,
                                             canvasOffsetY,
                                             alkaneStartsRising,
                                             alkeEneYneLineXOffsetPercentage,
                                             alkEneYneLineYOffsetPercentage,
                                             False)

                locant = CType(ast.complexSubstituents(complexSubstituentIndex).locants(locantIndex), Integer)
                rotation = 0
                cyclicRotation = 0
                translationVector = New Point(0, 0)

                ' --- finding available branch on parent ---

                ' sort branches so last branch will be least preferred branch
                ast.branches(locant - 1).takenBranches = bubbleSortDescending(ast.branches(locant - 1).takenBranches)
                ' branch to be used will be highest possible preferred branch, except for special case methane in which branch to be used = 1
                Dim branch As Integer
                branch = -1
                Dim foundHole As Boolean
                foundHole = False
                ' searching for 'holes' in array - e.g. if 1, 3 are taken branches, branch should be 2
                For branchIndex = 0 To ast.branches(locant - 1).takenBranches.Length - 1
                    If branchIndex + 1 < ast.branches(locant - 1).takenBranches(branchIndex) And Not foundHole Then
                        branch = branchIndex + 1
                        foundHole = True
                    End If
                Next
                ' if methane with nothing added to it, branch = 1
                If ast.branches(locant - 1).takenBranches.Length = 0 Then
                    branch = 1
                End If
                ' no holes found and not methane - branch position will have to be the least preferred position. e.g. if 1, 2, 3 are taken branches, branch should be 4
                If branch = -1 Then
                    branch = ast.branches(locant - 1).takenBranches(ast.branches(locant - 1).takenBranches.Length - 1) + 1
                End If

                ' mark branch as taken on parent
                ReDim Preserve ast.branches(locant - 1).takenBranches(ast.branches(locant - 1).takenBranches.Length)
                ast.branches(locant - 1).takenBranches(ast.branches(locant - 1).takenBranches.Length - 1) = branch

                ' --- rotating child as indicated by branch ---
                rotation = branchAngles(branch - 1)
                If newComplexPoints.ast.isCyclical Then
                    ' align cyclical complex substituent to be even and symmetrical either side of the connector
                    cyclicRotation = (Math.PI - (2 * Math.PI / newComplexPoints.ast.length)) / 2
                End If
                rotateOrganicCompountPoints(newComplexPoints, newComplexPoints.mainChains(0).points(0), 2 * Math.PI - (rotation + cyclicRotation))

                ' --- create connector ---
                Dim alkaneLength As Double
                alkaneLength = (alkaneRise ^ 2 + alkaneSpacing ^ 2) ^ (1 / 2)

                Dim startingPoint As New Point(ocPoints.mainChains(0).points(locant - 1).X, ocPoints.mainChains(0).points(locant - 1).Y)
                Dim endingPoint As New Point(startingPoint.X + alkaneLength, startingPoint.Y)
                endingPoint = rotatePoint(endingPoint, startingPoint, 2 * Math.PI - rotation)

                ' --- calculate needed translation and translate child to new location)
                translationVector = New Point(
                    endingPoint.X - newComplexPoints.mainChains(0).points(0).X,
                    endingPoint.Y - newComplexPoints.mainChains(0).points(0).Y
                )
                translateOrganicCompoundPoints(newComplexPoints, translationVector)

                ' --- rotate child depending on locant position in main chain ---
                Dim rotationAngle As Double
                rotationAngle = 0
                ' flip valley points (\/)
                If (alkaneStartsRising And locant Mod 2 <> 0) Or (Not alkaneStartsRising And locant Mod 2 = 0) Then
                    rotationAngle = Math.PI
                End If
                ' orient cycloalkane points
                If ast.isCyclical Then
                    rotationAngle = ((2 * Math.PI) / ast.length) * (locant - 1) - (2 * Math.PI) / (ast.length * 2)
                End If

                ' rotate connector
                endingPoint = rotatePoint(endingPoint, startingPoint, rotationAngle)

                ' rotate complex substituent
                rotateOrganicCompountPoints(newComplexPoints, startingPoint, rotationAngle)

                ' --- put together connector ---
                Dim newConnector As PointPath
                newConnector.points = {startingPoint, endingPoint}
                newConnector.line = New Polyline

                ReDim Preserve ocPoints.complexSubstituentConnectors(ocPoints.complexSubstituentConnectors.Length)
                ocPoints.complexSubstituentConnectors(ocPoints.complexSubstituentConnectors.Length - 1) = newConnector

                ' --- add data to ocPoints ---
                ' add eneYnePoints
                For index = 0 To newComplexPoints.eneYnePoints.Length - 1
                    ReDim Preserve ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length)
                    ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length - 1) = newComplexPoints.eneYnePoints(index)
                Next

                ' add mainChainPoints
                For index = 0 To newComplexPoints.mainChains.Length - 1
                    ReDim Preserve ocPoints.mainChains(ocPoints.mainChains.Length)
                    ocPoints.mainChains(ocPoints.mainChains.Length - 1) = newComplexPoints.mainChains(index)
                Next

                ' add simpleSubstituents
                For index = 0 To newComplexPoints.simpleSubstituents.Length - 1
                    ReDim Preserve ocPoints.simpleSubstituents(ocPoints.simpleSubstituents.Length)
                    ocPoints.simpleSubstituents(ocPoints.simpleSubstituents.Length - 1) = newComplexPoints.simpleSubstituents(index)
                Next

                ' add complexSubstituentConnectors
                For index = 0 To newComplexPoints.complexSubstituentConnectors.Length - 1
                    ReDim Preserve ocPoints.complexSubstituentConnectors(ocPoints.complexSubstituentConnectors.Length)
                    ocPoints.complexSubstituentConnectors(ocPoints.complexSubstituentConnectors.Length - 1) = newComplexPoints.complexSubstituentConnectors(index)
                Next
            Next
        Next

        ' --- simple substituents ---
        Dim createdSubstituents As FunctionalGroupPoints()
        For substituentIndex = 0 To ast.simpleSubstituents.Length - 1
            ' create SubstituentPoint for currently indexed substituent
            ' if multi locant substituent, multiple SubstituetPoints will be returned
            createdSubstituents = createSimpleSubstituent(ast, substituentIndex, functionalGroupDefinitions, bondLength, ocPoints.mainChains(0), alkaneStartsRising)
            For index = 0 To createdSubstituents.Length - 1
                ' add each created substituent point to simpleSubstituents array to be drawn later
                ReDim Preserve ocPoints.simpleSubstituents(ocPoints.simpleSubstituents.Length)
                ocPoints.simpleSubstituents(ocPoints.simpleSubstituents.Length - 1) = createdSubstituents(index)
            Next
        Next

        ' this is the compound is not put together until all the recursion is done
        If Not firstCall Then
            ocPoints.canvas = display
            Return ocPoints
        End If

        ' special case - methane
        If firstCall = True And ast.length = 1 And ast.simpleSubstituents.Length = 0 And ast.complexSubstituents.Length = 0 Then
            Dim ch3 As New TextBlock
            ch3.Background = Brushes.White
            ch3.Foreground = Brushes.DarkBlue
            ch3.Text = "CH₄"
            display.Children.Add(ch3)
        End If

        ' --- connecting the dots (generate polyline and textblocks from points and add to canvas) ---
        ' only happens on final exit from function
        If firstCall Then
            ' main chain
            For mainChainIndex = 0 To ocPoints.mainChains.Length - 1
                ocPoints.mainChains(mainChainIndex).line = generatePolylineFromPoints(ocPoints.mainChains(mainChainIndex).points)
                display.Children.Add(ocPoints.mainChains(mainChainIndex).line)
            Next

            ' ene yne points
            For index = 0 To ocPoints.eneYnePoints.Length - 1
                ocPoints.eneYnePoints(index).line = generatePolylineFromPoints(ocPoints.eneYnePoints(index).points)
                display.Children.Add(ocPoints.eneYnePoints(index).line)
            Next

            ' simple substituents
            For substituentIndex = 0 To ocPoints.simpleSubstituents.Length - 1
                ' points
                For pointPathIndex = 0 To ocPoints.simpleSubstituents(substituentIndex).points.Length - 1
                    ocPoints.simpleSubstituents(substituentIndex).points(pointPathIndex).line = generatePolylineFromPoints(ocPoints.simpleSubstituents(substituentIndex).points(pointPathIndex).points)
                    display.Children.Add(ocPoints.simpleSubstituents(substituentIndex).points(pointPathIndex).line)
                Next
                ' text
                For textIndex = 0 To ocPoints.simpleSubstituents(substituentIndex).representers.Length - 1
                    Dim newTextBlock As New TextBlock
                    newTextBlock.Text = ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).text
                    ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).textBlock = newTextBlock
                    display.Children.Add(ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).textBlock)

                    ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).textBlock.Measure(New Size(Double.PositiveInfinity, Double.PositiveInfinity))
                    ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).textBlock.Arrange(New Rect(ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).textBlock.DesiredSize))

                    ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).textBlock.Margin = _
                        New Thickness(ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).coords.X - (ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).textBlock.ActualWidth / 2), _
                                      ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).coords.Y - (ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).textBlock.ActualHeight / 2), 1000, 1000)

                    ' set textblock x coord to new calculated x coords
                    ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).coords.X = ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).coords.X - (ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).textBlock.ActualWidth / 2)
                    ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).coords.Y = ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).coords.Y - (ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).textBlock.ActualHeight / 2)

                    ' give textblock white background to block out lines covering it
                    ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).textBlock.Background = Brushes.White

                    ' colour textblock as specified
                    ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).textBlock.Foreground = New BrushConverter().ConvertFrom(ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).color)
                Next
            Next

            ' complex substituent connecting points
            For index = 0 To ocPoints.complexSubstituentConnectors.Length - 1
                ocPoints.complexSubstituentConnectors(index).line = generatePolylineFromPoints(ocPoints.complexSubstituentConnectors(index).points)
                display.Children.Add(ocPoints.complexSubstituentConnectors(index).line)
            Next


            ' --- offset points to fit in canvas  ---
            Dim topmostPoint As Double
            topmostPoint = 100000000
            Dim leftmostPoint As Double
            leftmostPoint = 100000000
            Dim bottommostPoint As Double
            bottommostPoint = -100000000
            Dim rightmostPoint As Double
            rightmostPoint = -100000000

            ' checking through main chain points
            For mainChainIndex = 0 To ocPoints.mainChains.Length - 1
                For pointIndex = 0 To ocPoints.mainChains(mainChainIndex).points.Length - 1
                    If ocPoints.mainChains(mainChainIndex).points(pointIndex).Y < topmostPoint Then
                        topmostPoint = ocPoints.mainChains(mainChainIndex).points(pointIndex).Y
                    End If
                    If ocPoints.mainChains(mainChainIndex).points(pointIndex).Y > bottommostPoint Then
                        bottommostPoint = ocPoints.mainChains(mainChainIndex).points(pointIndex).Y
                    End If
                    If ocPoints.mainChains(mainChainIndex).points(pointIndex).X < leftmostPoint Then
                        leftmostPoint = ocPoints.mainChains(mainChainIndex).points(pointIndex).X
                    End If
                    If ocPoints.mainChains(mainChainIndex).points(pointIndex).X > rightmostPoint Then
                        rightmostPoint = ocPoints.mainChains(mainChainIndex).points(pointIndex).X
                    End If
                Next
            Next

            ' checking through eneYne bars' points
            For eneYnePointIndex = 0 To ocPoints.eneYnePoints.Length - 1
                For pointIndex = 0 To ocPoints.eneYnePoints(eneYnePointIndex).points.Length - 1
                    If ocPoints.eneYnePoints(eneYnePointIndex).points(pointIndex).Y < topmostPoint Then
                        topmostPoint = ocPoints.eneYnePoints(eneYnePointIndex).points(pointIndex).Y
                    End If
                    If ocPoints.eneYnePoints(eneYnePointIndex).points(pointIndex).Y > bottommostPoint Then
                        bottommostPoint = ocPoints.eneYnePoints(eneYnePointIndex).points(pointIndex).Y
                    End If
                    If ocPoints.eneYnePoints(eneYnePointIndex).points(pointIndex).X < leftmostPoint Then
                        leftmostPoint = ocPoints.eneYnePoints(eneYnePointIndex).points(pointIndex).X
                    End If
                    If ocPoints.eneYnePoints(eneYnePointIndex).points(pointIndex).X > rightmostPoint Then
                        rightmostPoint = ocPoints.eneYnePoints(eneYnePointIndex).points(pointIndex).X
                    End If
                Next
            Next

            ' checking through simpleSubstituents points
            For substituentIndex = 0 To ocPoints.simpleSubstituents.Length - 1
                ' points
                For lineIndex = 0 To ocPoints.simpleSubstituents(substituentIndex).points.Length - 1
                    For pointIndex = 0 To ocPoints.simpleSubstituents(substituentIndex).points(lineIndex).points.Length - 1
                        If ocPoints.simpleSubstituents(substituentIndex).points(lineIndex).points(pointIndex).Y < topmostPoint Then
                            topmostPoint = ocPoints.simpleSubstituents(substituentIndex).points(lineIndex).points(pointIndex).Y
                        End If
                        If ocPoints.simpleSubstituents(substituentIndex).points(lineIndex).points(pointIndex).Y > bottommostPoint Then
                            bottommostPoint = ocPoints.simpleSubstituents(substituentIndex).points(lineIndex).points(pointIndex).Y
                        End If
                        If ocPoints.simpleSubstituents(substituentIndex).points(lineIndex).points(pointIndex).X < leftmostPoint Then
                            leftmostPoint = ocPoints.simpleSubstituents(substituentIndex).points(lineIndex).points(pointIndex).X
                        End If
                        If ocPoints.simpleSubstituents(substituentIndex).points(lineIndex).points(pointIndex).X > rightmostPoint Then
                            rightmostPoint = ocPoints.simpleSubstituents(substituentIndex).points(lineIndex).points(pointIndex).X
                        End If
                    Next
                Next
                ' text
                For textIndex = 0 To ocPoints.simpleSubstituents(substituentIndex).representers.Length - 1
                    If ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).coords.Y < topmostPoint Then
                        topmostPoint = ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).coords.Y
                    End If
                    If ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).coords.Y > bottommostPoint Then
                        bottommostPoint = ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).coords.Y
                    End If
                    If ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).coords.X < leftmostPoint Then
                        leftmostPoint = ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).coords.X
                    End If
                    If ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).coords.X > rightmostPoint Then
                        rightmostPoint = ocPoints.simpleSubstituents(substituentIndex).representers(textIndex).coords.X
                    End If
                Next
            Next

            ' checking through complexSubstituent points
            For complexSubstituentConnectorIndex = 0 To ocPoints.complexSubstituentConnectors.Length - 1
                For pointIndex = 0 To ocPoints.complexSubstituentConnectors(complexSubstituentConnectorIndex).points.Length - 1
                    If ocPoints.complexSubstituentConnectors(complexSubstituentConnectorIndex).points(pointIndex).Y < topmostPoint Then
                        topmostPoint = ocPoints.mainChains(complexSubstituentConnectorIndex).points(pointIndex).Y
                    End If
                    If ocPoints.complexSubstituentConnectors(complexSubstituentConnectorIndex).points(pointIndex).Y > bottommostPoint Then
                        bottommostPoint = ocPoints.mainChains(complexSubstituentConnectorIndex).points(pointIndex).Y
                    End If
                    If ocPoints.complexSubstituentConnectors(complexSubstituentConnectorIndex).points(pointIndex).X < leftmostPoint Then
                        leftmostPoint = ocPoints.mainChains(complexSubstituentConnectorIndex).points(pointIndex).X
                    End If
                    If ocPoints.complexSubstituentConnectors(complexSubstituentConnectorIndex).points(pointIndex).X > rightmostPoint Then
                        rightmostPoint = ocPoints.mainChains(complexSubstituentConnectorIndex).points(pointIndex).X
                    End If
                Next
            Next

            For Each child In display.Children
                Canvas.SetTop(child, canvasOffsetY + -1 * topmostPoint)
                Canvas.SetLeft(child, canvasOffsetX + -1 * leftmostPoint)
            Next

            display.Width = rightmostPoint + canvasOffsetX * 2
            display.Height = bottommostPoint + canvasOffsetY * 2

            If leftmostPoint < 0 Then
                display.Width = display.Width + -1 * leftmostPoint
            End If
            If topmostPoint < 0 Then
                display.Height = display.Height + -1 * topmostPoint
            End If
        End If
        ocPoints.canvas = display
        Return ocPoints
    End Function

    ''' <summary>
    ''' Given a canvas element, a filetype of either png or jpg, and a file path, export the canvas as an image
    ''' Can be used with a rendered AST canvas to export a rendered organicCompound
    ''' </summary>
    ''' <param name="canvas">The canvas to be rendered</param>
    ''' <param name="fileType">A string containing either 'png' or 'jpg' signifying whether the image should be exported in PNG or JPEG format</param>
    ''' <param name="filePath">The path the image should be exported to. Existing images with the same name will be overwritten</param>
    ''' <remarks></remarks>
    Public Sub exportCanvasAsImage(ByVal canvas As Canvas, ByVal fileType As String, ByVal filePath As String)
        Dim transform As Transform = canvas.LayoutTransform
        canvas.LayoutTransform = Nothing

        Dim size = New Size(canvas.Width, canvas.Height)
        canvas.Measure(size)
        canvas.Arrange(New Rect(size))

        Dim renderBitmap = New RenderTargetBitmap(canvas.Width, canvas.Height, 96D, 96D, PixelFormats.Default)
        renderBitmap.Render(canvas)

        Using fileStream As New System.IO.FileStream(filePath, IO.FileMode.Create)
            Dim encoder As BitmapEncoder
            Select Case fileType
                Case "png"
                    encoder = New PngBitmapEncoder()
                Case Else
                    encoder = New JpegBitmapEncoder()
            End Select

            encoder.Frames.Add(BitmapFrame.Create(renderBitmap))
            encoder.Save(fileStream)
        End Using

        canvas.LayoutTransform = transform
    End Sub
End Module