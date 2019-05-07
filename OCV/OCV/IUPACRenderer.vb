Module IUPACRenderer

    ' index of Point in points + 1 = its locant
    ' polygon only used when cyclical
    Public Structure PointPath
        Public isCyclical As Boolean
        Public points As Point()
        Public line As Polyline
    End Structure

    ' index of simpleSubstituent + 1 = its locant
    ' index of complexSubstituent + 1 = its locant
    Public Structure OrganicCompoundPoints
        Public mainChain As PointPath
        Public eneYnePoints As PointPath()
        Public simpleSubstituents As IUPACParser.FunctionalGroupDefinition()
        Public complexSubstituents As OrganicCompoundPoints()
    End Structure


    Private Function generatePolylineFromPoints(ByVal points As Point()) As Polyline
        Dim polyline As Polyline = New Polyline()
        polyline.Stroke = Brushes.Black
        polyline.StrokeThickness = 1

        For index = 0 To points.Length - 1
            polyline.Points.Add(points(index))
        Next

        Return polyline
    End Function

    ' assumes valid base
    Public Function renderAST(ByVal ast As IUPACParser.ASTAlkaneBase, ByVal resolutionXY As Integer(), _
                              Optional ByVal scale As Double = 1,
                              Optional ByVal alkaneSpacing As Integer = 30,
                              Optional ByVal alkaneRise As Integer = 17,
                              Optional ByVal canvasOffsetX As Integer = 30,
                              Optional ByVal canvasOffsetY As Integer = 30,
                              Optional ByVal alkaneStartsRising As Boolean = True,
                              Optional ByVal alkeEneYneLineXOffsetPercentage As Double = 2 / 30,
                              Optional ByVal alkEneYneLineYOffsetPercentage As Double = 4 / 17) As Canvas
        ' initialise canvas
        Dim display As New Canvas
        display.Width = resolutionXY(0)
        display.Height = resolutionXY(1)
        display.Background = Brushes.White

        ' initialise organicCompoundPoints for current alkane
        Dim mainChainPointPath As PointPath
        mainChainPointPath.points = {}
        mainChainPointPath.line = New Polyline()

        Dim ocPoints As OrganicCompoundPoints
        ocPoints.mainChain = mainChainPointPath
        ocPoints.eneYnePoints = {}
        ocPoints.simpleSubstituents = {}
        ocPoints.complexSubstituents = {}

        ' set scale
        alkaneSpacing = alkaneSpacing * scale
        alkaneRise = alkaneRise * scale

        ' generate ene and yne locants
        Dim eneLocants As Integer()
        eneLocants = {}
        Dim yneLocants As Integer()
        yneLocants = {}
        For index = 0 To ast.simpleSubstituents.Length - 1
            If ast.simpleSubstituents(index).type.type = "alkene" Then
                For locantIndex = 0 To ast.simpleSubstituents(index).locants.Length - 1
                    ReDim Preserve eneLocants(eneLocants.Length)
                    eneLocants(eneLocants.Length - 1) = CType(ast.simpleSubstituents(index).locants(locantIndex), Integer)
                Next
            Else
                If ast.simpleSubstituents(index).type.type = "alkyne" Then
                    For locantIndex = 0 To ast.simpleSubstituents(index).locants.Length - 1
                        ReDim Preserve yneLocants(yneLocants.Length)
                        yneLocants(yneLocants.Length - 1) = CType(ast.simpleSubstituents(index).locants(locantIndex), Integer)
                    Next
                End If
            End If
        Next


        ' 1. create main chain point path
        Dim isRising As Boolean
        isRising = alkaneStartsRising

        Dim previousPoint As Point = New Point()
        Dim newPoint As Point = New Point(0, 0)

        Dim currentLocant As Integer

        ' straight chains are dealt with differently to cyclical chains
        If Not ast.isCyclical Then
            ' straight chain alkane base
            ocPoints.mainChain.isCyclical = False
            For index = 0 To ast.length - 1
                ' as the 'locant' of the current point is the index+1 (0 indexing or arrays vs 1 indexing of locants)
                currentLocant = index + 1

                ' if the current locant is an alkyne or an alkene, create polylines for it (dashes above and below points)
                If yneLocants.Contains(currentLocant - 1) Or eneLocants.Contains(currentLocant - 1) Then
                    ' init variables for above and below bars (dim so that it is created every time it needs to be, no linking)
                    Dim eneYneAboveBar As PointPath
                    eneYneAboveBar.isCyclical = False
                    eneYneAboveBar.points = {Nothing, Nothing}
                    eneYneAboveBar.line = New Polyline()

                    Dim eneYneBelowBar As PointPath
                    eneYneBelowBar.isCyclical = False
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
                ReDim Preserve ocPoints.mainChain.points(ocPoints.mainChain.points.Length)
                ocPoints.mainChain.points(ocPoints.mainChain.points.Length - 1) = newPoint

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
            ocPoints.mainChain.isCyclical = True

            ' will be regular polygon with n sides, can be represented as n congruent iscosceles triangles with tips all at one point
            ' calculate interior (turning) angle
            Dim turnAngle As Double
            turnAngle = (2 * Math.PI) / ast.length

            ' calculate length of each side (pythagoras)
            Dim sideLength As Double
            sideLength = (alkaneSpacing ^ 2 + alkaneRise ^ 2) ^ (1 / 2)

            ' calculate "radius" of polygon (length of equal side in the iscosceles triangle) (cosine rule)
            Dim polygonRadius As Double
            polygonRadius = Math.Sqrt((sideLength ^ 2) / (2 - 2 * Math.Cos(turnAngle)))

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

                If eneLocants.Contains(currentLocant - 1) Or yneLocants.Contains(currentLocant - 1) Then
                    ' draw ene line or bottom yne line
                    ReDim Preserve ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length)
                    ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length - 1).isCyclical = False
                    ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length - 1).points = {
                        New Point(previousAlkenePoint.X, previousAlkenePoint.Y),
                        New Point(currentAlkenePoint.X, currentAlkenePoint.Y)
                    }

                    ' draw top yne line if needed
                    If yneLocants.Contains(currentLocant - 1) Then

                        Dim distance As Double
                        distance = topBottomDifference * 2

                        ReDim Preserve ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length)
                        ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length - 1).isCyclical = False

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
                ReDim Preserve ocPoints.mainChain.points(ocPoints.mainChain.points.Length)
                ocPoints.mainChain.points(ocPoints.mainChain.points.Length - 1) = pointToAdd
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
                ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length - 1).isCyclical = False
                ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length - 1).points = {
                    New Point(previousAlkenePoint.X, previousAlkenePoint.Y),
                    New Point(currentAlkenePoint.X, currentAlkenePoint.Y)
                }

                ' draw top yne line if needed
                If yneLocants.Contains(currentLocant) Then

                    Dim distance As Double
                    distance = topBottomDifference * 2

                    ReDim Preserve ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length)
                    ocPoints.eneYnePoints(ocPoints.eneYnePoints.Length - 1).isCyclical = False

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
            ReDim Preserve ocPoints.mainChain.points(ocPoints.mainChain.points.Length)
            ocPoints.mainChain.points(ocPoints.mainChain.points.Length - 1) = startingPoint
        End If

        ' generate polyline from points and add to canvas
        ocPoints.mainChain.line = generatePolylineFromPoints(ocPoints.mainChain.points)
        display.Children.Add(ocPoints.mainChain.line)
        For index = 0 To ocPoints.eneYnePoints.Length - 1
            ocPoints.eneYnePoints(index).line = generatePolylineFromPoints(ocPoints.eneYnePoints(index).points)
            display.Children.Add(ocPoints.eneYnePoints(index).line)
        Next

        ' offset negative points by finding most negative point - todo: should be recursive and handle points of nested substituents and normal substituents and alkene/yne lines
        ' TODO: check through alkane/ene lines 
        Dim negativeYOffset As Integer
        negativeYOffset = 100000000
        ' checking through main chain points
        For pointIndex = 0 To ocPoints.mainChain.points.Length - 1
            If ocPoints.mainChain.points(pointIndex).Y < negativeYOffset Then
                negativeYOffset = ocPoints.mainChain.points(pointIndex).Y
            End If
        Next
        ' checking through eneYne bars' points
        For eneYnePointIndex = 0 To ocPoints.eneYnePoints.Length - 1
            For pointIndex = 0 To ocPoints.eneYnePoints(eneYnePointIndex).points.Length - 1
                If ocPoints.eneYnePoints(eneYnePointIndex).points(pointIndex).Y < negativeYOffset Then
                    negativeYOffset = ocPoints.eneYnePoints(eneYnePointIndex).points(pointIndex).Y
                End If
            Next
        Next
        negativeYOffset = -1 * negativeYOffset

        Dim negativeXOffset As Integer
        negativeXOffset = 100000000
        ' checking through main chain points
        For pointIndex = 0 To ocPoints.mainChain.points.Length - 1
            If ocPoints.mainChain.points(pointIndex).X < negativeXOffset Then
                negativeXOffset = ocPoints.mainChain.points(pointIndex).X
            End If
        Next
        ' checking through eneYne bars' points
        For eneYnePointIndex = 0 To ocPoints.eneYnePoints.Length - 1
            For pointIndex = 0 To ocPoints.eneYnePoints(eneYnePointIndex).points.Length - 1
                If ocPoints.eneYnePoints(eneYnePointIndex).points(pointIndex).X < negativeXOffset Then
                    negativeXOffset = ocPoints.eneYnePoints(eneYnePointIndex).points(pointIndex).X
                End If
            Next
        Next
        negativeXOffset = -1 * negativeXOffset

        For Each child In display.Children
            Canvas.SetTop(child, canvasOffsetY + negativeYOffset)
            Canvas.SetLeft(child, canvasOffsetX + negativeXOffset)
        Next
        Return display
    End Function

    ''' <summary>
    ''' Given a canvas element, a filetype of either png or jpg, and a file pat, export it as an image
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

