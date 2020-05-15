Public Class Form1
    Dim go1 As Boolean = True
    Dim go2 As Boolean = True
    Dim go3 As Boolean = True
    Dim go4 As Boolean = True
    Dim go5 As Boolean = True
    Dim go6 As Boolean = True
    Dim go7 As Boolean = True
    Dim go8 As Boolean = True
    Dim go9 As Boolean = True
    Dim go10 As Boolean = True
    Dim go11 As Boolean = True
    Dim go12 As Boolean = True
    Dim go13 As Boolean = True
    Dim go14 As Boolean = True
    Dim go15 As Boolean = True
    Dim go16 As Boolean = True
    Dim go17 As Boolean = True
    Dim go18 As Boolean = True
    Dim go19 As Boolean = True
    Dim go20 As Boolean = True
    Dim go21 As Boolean = True
    Dim go22 As Boolean = True
    Dim go23 As Boolean = True
    Dim go24 As Boolean = True
    Dim go26 As Boolean = True
    Dim go27 As Boolean = True
    Dim go28 As Boolean = True
    Dim go29 As Boolean = True
    Dim go30 As Boolean = True
    Dim go31 As Boolean = True
    Dim go25 As Boolean = True
    Dim go32 As Boolean = True
    Dim control As Single = 1
    Dim control2 As Single = 1
    Dim control1 As Single = 1
    Function TimeAllocate(ByVal NoOfCars As Integer, ByVal TimeOfWaiting As Integer, ByVal TimeToAllocate As Integer, ByVal text11 As TextBox)

        Dim MemCar_VeryFew As Double
        Dim MemCar_few As Double
        Dim MemCar_Normal As Double
        Dim MemCar_High As Double


        Dim MemTimeW_verylow As Double
        Dim MemTimeW_low As Double
        Dim MemTimeW_Normal As Double
        Dim MemTimeW_High As Double


        Dim MTimeAllocate_verylow As Double
        Dim MTimeAllocate_low As Double
        Dim MTimeAllocate_Normal As Double
        Dim MTimeAllocate_High As Double
        If NoOfCars > 0 And NoOfCars <= 5 Then
            MemCar_VeryFew = -0.2 * NoOfCars + 1
            MemCar_few = 0.2 * NoOfCars
            If TimeOfWaiting >= 0 And TimeOfWaiting <= 40 Then

                MemTimeW_verylow = (-0.025 * TimeOfWaiting) + 1
                MemTimeW_low = 0.025 * TimeOfWaiting

                'Compare all the Membership value to get membership value of timetoallocate
                If MemCar_VeryFew <> 0 And MemTimeW_verylow <> 0 Then
                    MTimeAllocate_verylow = Math.Min(MemTimeW_verylow, MemCar_VeryFew)

                End If
                If MemCar_VeryFew <> 0 And MemTimeW_low <> 0 Then
                    If MTimeAllocate_verylow < Math.Min(MemTimeW_low, MemCar_VeryFew) Then
                        MTimeAllocate_verylow = Math.Min(MemTimeW_low, MemCar_VeryFew)
                    End If
                End If
                If MemCar_few <> 0 And MemTimeW_verylow <> 0 Then
                    MTimeAllocate_low = Math.Min(MemCar_few, MemTimeW_verylow)

                End If
                If MemCar_few <> 0 And MemTimeW_low <> 0 Then
                    If MTimeAllocate_low < Math.Min(MemCar_few, MemTimeW_low) Then
                        MTimeAllocate_low = Math.Min(MemCar_few, MemTimeW_low)
                    End If


                End If
                TimeToAllocate = ((((MTimeAllocate_verylow - 1) / -0.025) + (MTimeAllocate_low / 0.025) + ((MTimeAllocate_low - 2) / -0.025)) / 3) / 2
                text11.Text = TimeToAllocate.ToString
            ElseIf TimeOfWaiting >= 41 And TimeOfWaiting <= 80 Then
                MemTimeW_low = -0.025 * TimeOfWaiting + 2
                MemTimeW_Normal = 0.025 * TimeOfWaiting - 1

                If MemCar_VeryFew <> 0 And MemTimeW_low <> 0 Then
                    MTimeAllocate_verylow = Math.Min(MemCar_VeryFew, MemTimeW_low)
                End If

                If MemCar_VeryFew <> 0 And MemTimeW_Normal <> 0 Then
                    MTimeAllocate_low = Math.Min(MemCar_VeryFew, MemTimeW_Normal)
                End If

                If MemCar_few <> 0 And MemTimeW_low <> 0 Then
                    If MTimeAllocate_low < Math.Min(MemCar_few, MemTimeW_low) Then
                        MTimeAllocate_low = Math.Min(MemCar_few, MemTimeW_low)
                    End If
                End If
                If MemCar_few <> 0 And MemTimeW_Normal <> 0 Then
                    If MTimeAllocate_low < Math.Min(MemCar_few, MemTimeW_Normal) Then
                        MTimeAllocate_low = Math.Min(MemCar_few, MemTimeW_Normal)
                    End If
                End If
                TimeToAllocate = ((((MTimeAllocate_verylow - 1) / -0.025) + (MTimeAllocate_low / 0.025) + ((MTimeAllocate_low - 2) / -0.025)) / 3) / 2
                text11.Text = TimeToAllocate.ToString
            ElseIf TimeOfWaiting >= 81 And TimeOfWaiting <= 120 Then
                MemTimeW_Normal = -0.025 * TimeOfWaiting + 3
                MemTimeW_High = 0.025 * TimeOfWaiting - 2

                If MemCar_VeryFew <> 0 And MemTimeW_Normal <> 0 Then
                    MTimeAllocate_low = Math.Min(MemCar_VeryFew, MemTimeW_Normal)
                End If
                If MemCar_VeryFew <> 0 And MemTimeW_High <> 0 Then
                    If MTimeAllocate_low < Math.Min(MemCar_VeryFew, MemTimeW_High) Then
                        MTimeAllocate_low = Math.Min(MemCar_VeryFew, MemTimeW_High)
                    End If
                End If
                If MemCar_few <> 0 And MemTimeW_Normal <> 0 Then
                    If MTimeAllocate_low < Math.Min(MemCar_few, MemTimeW_Normal) Then
                        MTimeAllocate_low = Math.Min(MemCar_few, MemTimeW_Normal)
                    End If
                End If
                If MemCar_few <> 0 And MemTimeW_High <> 0 Then
                    MTimeAllocate_Normal = Math.Min(MemCar_few, MemTimeW_High)
                End If
                TimeToAllocate = ((((MTimeAllocate_verylow - 1) / -0.025) + (MTimeAllocate_low / 0.025) + ((MTimeAllocate_low - 2) / -0.025) + ((MTimeAllocate_Normal + 1) / 0.025) + ((MTimeAllocate_Normal - 3) / -0.025)) / 5) / 2

                text11.Text = TimeToAllocate.ToString
            ElseIf TimeOfWaiting > 120 Then
                text11.Text = "invalid Time of waiting"
            End If

        ElseIf NoOfCars >= 6 And NoOfCars <= 10 Then
            MemCar_few = -0.2 * NoOfCars + 2
            MemCar_Normal = 0.2 * NoOfCars - 1
            'Start here
            If TimeOfWaiting >= 0 And TimeOfWaiting <= 40 Then
                MemTimeW_verylow = (-0.025 * TimeOfWaiting) + 1
                MemTimeW_low = 0.025 * TimeOfWaiting
                If MemCar_few <> 0 And MemTimeW_verylow <> 0 Then
                    MTimeAllocate_verylow = Math.Min(MemCar_few, MemTimeW_verylow)
                End If
                If MemCar_few <> 0 And MemTimeW_low <> 0 Then
                    MTimeAllocate_low = Math.Min(MemCar_few, MemTimeW_low)
                End If
                If MemCar_Normal <> 0 And MemTimeW_verylow <> 0 Then
                    If MTimeAllocate_low < Math.Min(MemCar_Normal, MemTimeW_verylow) Then
                        MTimeAllocate_low = Math.Min(MemCar_Normal, MemTimeW_verylow)
                    End If
                    If MemCar_Normal <> 0 And MemTimeW_low <> 0 Then
                        If MTimeAllocate_low < Math.Min(MemCar_Normal, MemTimeW_low) Then
                            MTimeAllocate_low = Math.Min(MemCar_Normal, MemTimeW_low)
                        End If
                    End If
                End If
                TimeToAllocate = ((((MTimeAllocate_verylow - 1) / -0.025) + (MTimeAllocate_low / 0.025) + ((MTimeAllocate_low - 2) / -0.025)) / 3) / 2
                text11.Text = TimeToAllocate.ToString
            ElseIf TimeOfWaiting >= 41 And TimeOfWaiting <= 80 Then
                MemTimeW_low = -0.025 * TimeOfWaiting + 2
                MemTimeW_Normal = 0.025 * TimeOfWaiting - 1
                If MemCar_few <> 0 And MemTimeW_low <> 0 Then
                    MTimeAllocate_low = Math.Min(MemCar_few, MemTimeW_low)
                End If
                If MemCar_few <> 0 And MemTimeW_Normal <> 0 Then
                    If MTimeAllocate_low < Math.Min(MemCar_few, MemTimeW_Normal) Then
                        MTimeAllocate_low = Math.Min(MemCar_few, MemTimeW_Normal)
                    End If
                    If MemCar_Normal <> 0 And MemTimeW_low <> 0 Then
                        If MTimeAllocate_low < Math.Min(MemCar_Normal, MemTimeW_low) Then
                            MTimeAllocate_low = Math.Min(MemCar_Normal, MemTimeW_low)
                        End If
                        If MemCar_Normal <> 0 And MemTimeW_Normal <> 0 Then
                            MTimeAllocate_Normal = Math.Min(MemCar_Normal, MemTimeW_Normal)
                        End If
                    End If
                    TimeToAllocate = (((MTimeAllocate_low / 0.025) + ((MTimeAllocate_low - 2) / -0.025) + ((MTimeAllocate_Normal + 1) / 0.025) + ((MTimeAllocate_Normal - 3) / -0.025)) / 4) / 2
                    text11.Text = TimeToAllocate.ToString
                End If
            ElseIf TimeOfWaiting >= 81 And TimeOfWaiting <= 120 Then
                MemTimeW_Normal = -0.025 * TimeOfWaiting + 3
                MemTimeW_High = 0.025 * TimeOfWaiting - 2
                If MemCar_few <> 0 And MemTimeW_Normal <> 0 Then
                    MTimeAllocate_low = Math.Min(MemCar_few, MemTimeW_Normal)
                End If
                If MemCar_few <> 0 And MemTimeW_High <> 0 Then
                    MTimeAllocate_Normal = Math.Min(MemCar_few, MemTimeW_High)
                End If
                If MemCar_Normal <> 0 And MemTimeW_Normal <> 0 Then
                    If MTimeAllocate_Normal < Math.Min(MemCar_Normal, MemTimeW_Normal) Then
                        MTimeAllocate_Normal = Math.Min(MemCar_Normal, MemTimeW_Normal)
                    End If

                End If
                If MemCar_Normal <> 0 And MemTimeW_High <> 0 Then
                    If MTimeAllocate_Normal < Math.Min(MemCar_Normal, MemTimeW_High) Then
                        MTimeAllocate_Normal = Math.Min(MemCar_Normal, MemTimeW_High)
                    End If
                End If
                TimeToAllocate = (((MTimeAllocate_low / 0.025) + ((MTimeAllocate_low - 2) / -0.025) + ((MTimeAllocate_Normal + 1) / 0.025) + ((MTimeAllocate_Normal - 3) / -0.025)) / 4) / 2
                text11.Text = TimeToAllocate.ToString
            ElseIf TimeOfWaiting > 120 Then
                text11.Text = "invalid Time of waiting"
            End If
        ElseIf NoOfCars >= 11 And NoOfCars <= 16 Then
            MemCar_Normal = -0.2 * NoOfCars + 3
            MemCar_High = 0.2 * NoOfCars - 2
            'stsrt here
            If TimeOfWaiting >= 0 And TimeOfWaiting <= 40 Then
                MemTimeW_verylow = (-0.025 * TimeOfWaiting) + 1
                MemTimeW_low = 0.025 * TimeOfWaiting
                If MemCar_Normal <> 0 And MemTimeW_verylow <> 0 Then
                    MTimeAllocate_low = Math.Min(MemCar_Normal, MemTimeW_verylow)
                End If
                If MemCar_Normal <> 0 And MemTimeW_low <> 0 Then
                    If MTimeAllocate_low < Math.Min(MemCar_Normal, MemTimeW_low) Then
                        MTimeAllocate_low = Math.Min(MemCar_Normal, MemTimeW_low)
                    End If
                End If
                If MemCar_High <> 0 And MemTimeW_verylow <> 0 Then
                    MTimeAllocate_Normal = Math.Min(MemCar_High, MemTimeW_verylow)
                End If
                If MemCar_High <> 0 And MemTimeW_low <> 0 Then
                    If MTimeAllocate_Normal < Math.Min(MemCar_High, MemTimeW_low) Then
                        MTimeAllocate_Normal = Math.Min(MemCar_High, MemTimeW_low)
                    End If
                End If
                TimeToAllocate = (((MTimeAllocate_low / 0.025) + ((MTimeAllocate_low - 2) / -0.025) + ((MTimeAllocate_Normal + 1) / 0.025) + ((MTimeAllocate_Normal - 3) / -0.025)) / 4) / 2
                text11.Text = TimeToAllocate.ToString

            ElseIf TimeOfWaiting >= 41 And TimeOfWaiting <= 80 Then
                MemTimeW_low = -0.025 * TimeOfWaiting + 2
                MemTimeW_Normal = 0.025 * TimeOfWaiting - 1
                If MemCar_Normal <> 0 And MemTimeW_low <> 0 Then
                    MTimeAllocate_low = Math.Min(MemCar_Normal, MemTimeW_low)
                End If
                If MemCar_Normal <> 0 And MemTimeW_Normal <> 0 Then
                    MTimeAllocate_Normal = Math.Min(MemCar_Normal, MemTimeW_Normal)
                End If
                If MemCar_High <> 0 And MemTimeW_low <> 0 Then
                    If MTimeAllocate_Normal < Math.Min(MemCar_High, MemTimeW_low) Then
                        MTimeAllocate_Normal = Math.Min(MemCar_High, MemTimeW_low)
                    End If
                End If
                If MemCar_High <> 0 And MemTimeW_Normal Then
                    If MTimeAllocate_Normal < Math.Min(MemCar_High, MemTimeW_Normal) Then
                        MTimeAllocate_Normal = Math.Min(MemCar_High, MemTimeW_Normal)
                    End If
                End If
                TimeToAllocate = (((MTimeAllocate_low / 0.025) + ((MTimeAllocate_low - 2) / -0.025) + ((MTimeAllocate_Normal + 1) / 0.025) + ((MTimeAllocate_Normal - 3) / -0.025)) / 4) / 2
                text11.Text = TimeToAllocate.ToString
            ElseIf TimeOfWaiting >= 81 And TimeOfWaiting <= 120 Then
                MemTimeW_Normal = -0.025 * TimeOfWaiting + 3
                MemTimeW_High = 0.025 * TimeOfWaiting - 2
                'MessageBox.Show(MemCar_Normal.ToString)
                If MemCar_Normal <> 0 And MemTimeW_Normal <> 0 Then
                    MTimeAllocate_Normal = Math.Min(MemCar_Normal, MemTimeW_Normal)
                    'MessageBox.Show(MemTimeW_Normal.ToString)
                End If
                If MemCar_Normal <> 0 And MemTimeW_High <> 0 Then
                    If MTimeAllocate_Normal < Math.Min(MemCar_Normal, MemTimeW_High) Then
                        MTimeAllocate_Normal = Math.Min(MemCar_Normal, MemTimeW_High)
                    End If
                End If
                If MemCar_High <> 0 And MemTimeW_Normal <> 0 Then
                    If MTimeAllocate_Normal < Math.Min(MemCar_Normal, MemTimeW_Normal) Then
                        MTimeAllocate_Normal = Math.Min(MemCar_Normal, MemTimeW_Normal)
                    End If
                End If
                If MemCar_High <> 0 And MemTimeW_High <> 0 Then
                    MTimeAllocate_High = Math.Min(MemCar_High, MemTimeW_High)
                End If
                TimeToAllocate = ((((MTimeAllocate_Normal + 1) / 0.025) + ((MTimeAllocate_Normal - 3) / -0.025) + ((MTimeAllocate_High + 2) / 0.025)) / 3) / 2

                text11.Text = TimeToAllocate.ToString
            ElseIf TimeOfWaiting > 120 Then
                text11.Text = "invalid Time of waiting"
            End If


        ElseIf NoOfCars = 0 Then
            TimeToAllocate = 0
            text11.Text = TimeToAllocate.ToString
        End If




    End Function
    Private Sub PictureBox2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles car1.Click

    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Simulate.Click
        
        TimeInA.Start()
        CarATimer.Start()



    End Sub
    Function ChPositionA1(ByRef picture As PictureBox, ByRef count As Single)
        count = control

        If (picture.Left < 365 Or picture.Left = 365) And count = 0 Then
            picture.Left = picture.Left
        Else
            picture.Left += 1
        End If

    End Function
    Function ChPositionB1(ByRef picture As PictureBox)
        picture.Left -= 2

    End Function
    Function ChPositionB2(ByRef picture1 As PictureBox, ByRef picture2 As PictureBox, ByRef pos As Boolean, ByRef count1 As Single)
        If (picture1.Left = 508 Or picture1.Left > 508) And count1 = 0 Then
            picture1.Left = picture1.Left
        Else
            If pos = True Then
                picture1.Left -= 2
            End If
            If picture1.Left < 419 Or picture1.Left = 419 Then
                picture1.Image = picture2.Image
                picture1.Top += 2
                pos = False
            End If
        End If
    End Function
    Function ChPositionA2(ByRef picture1 As PictureBox, ByRef picture2 As PictureBox, ByRef pos As Boolean)
        If pos = True Then
            picture1.Left += 1
        End If
        If picture1.Left > 373 Or picture1.Left = 373 Then
            picture1.Image = PictureBox49.Image
            picture1.Top += 1
            pos = False
        End If
    End Function
    Function ChPositionC1(ByRef picture1 As PictureBox, ByRef picture2 As PictureBox, ByRef pos As Boolean)
        If pos = True Then
            picture1.Top -= 2
        End If
        If picture1.Top = 297 Or picture1.Top < 297 Then
            picture1.Image = car1.Image
            picture1.Left += 2
            pos = False
        End If

    End Function
    Function ChPositionC2(ByRef picture1 As PictureBox, ByRef picture2 As PictureBox, ByRef pos As Boolean, ByRef count1 As Single)
        If (picture1.Top = 343 Or picture1.Top > 343) And count1 = 0 Then
            picture1.Top = picture1.Top
        Else

            If pos = True Then
                picture1.Top -= 3
            End If
            If picture1.Top = 200 Or picture1.Top < 200 Then
                picture1.Image = PictureBox1.Image
                picture1.Left -= 3
                pos = False
            End If
        End If
    End Function
    Function LoadCars(ByRef a1 As Integer, ByRef a As PictureBox, ByVal b As PictureBox, ByVal c As PictureBox, ByVal d As PictureBox, ByVal e As PictureBox, ByVal f As PictureBox, ByVal g As PictureBox, ByVal h As PictureBox, ByVal i As PictureBox, ByVal j As PictureBox, ByVal k As PictureBox, ByVal l As PictureBox, ByVal m As PictureBox, ByVal n As PictureBox, ByVal o As PictureBox, ByVal p As PictureBox)
        a.Visible = False
        b.Visible = False
        c.Visible = False
        d.Visible = False
        e.Visible = False
        f.Visible = False
        g.Visible = False
        h.Visible = False
        i.Visible = False
        j.Visible = False
        k.Visible = False
        l.Visible = False
        m.Visible = False
        n.Visible = False
        o.Visible = False
        p.Visible = False


        If a1 < 0 Or a1 > 16 Or a1 = 0 Then
            MessageBox.Show("The numbers of cars is invalid")
        Else
            Select Case a1
                Case 1 : a.Visible = True
                Case 2 : b.Visible = True
                    c.Visible = True
                Case 3 : a.Visible = True
                    b.Visible = True
                    c.Visible = True
                Case 4 : a.Visible = True
                    b.Visible = True
                    c.Visible = True
                    d.Visible = True
                Case 5 : a.Visible = True
                    b.Visible = True
                    c.Visible = True
                    d.Visible = True
                    e.Visible = True
                Case 6 : a.Visible = True
                    b.Visible = True
                    c.Visible = True
                    d.Visible = True
                    e.Visible = True
                    f.Visible = True

                Case 7 : a.Visible = True
                    b.Visible = True
                    c.Visible = True
                    d.Visible = True
                    e.Visible = True
                    f.Visible = True
                    g.Visible = True
                Case 8 : a.Visible = True
                    b.Visible = True
                    c.Visible = True
                    d.Visible = True
                    e.Visible = True
                    f.Visible = True
                    g.Visible = True
                    h.Visible = True
                Case 9 : a.Visible = True
                    b.Visible = True
                    c.Visible = True
                    d.Visible = True
                    e.Visible = True
                    f.Visible = True
                    g.Visible = True
                    h.Visible = True
                    i.Visible = True
                Case 10 : a.Visible = True
                    b.Visible = True
                    c.Visible = True
                    d.Visible = True
                    e.Visible = True
                    f.Visible = True
                    g.Visible = True
                    h.Visible = True
                    i.Visible = True
                    j.Visible = True
                Case 11 : a.Visible = True
                    b.Visible = True
                    c.Visible = True
                    d.Visible = True
                    e.Visible = True
                    f.Visible = True
                    g.Visible = True
                    h.Visible = True
                    i.Visible = True
                    j.Visible = True
                    k.Visible = True
                Case 12 : a.Visible = True
                    b.Visible = True
                    c.Visible = True
                    d.Visible = True
                    e.Visible = True
                    f.Visible = True
                    g.Visible = True
                    h.Visible = True
                    i.Visible = True
                    j.Visible = True
                    k.Visible = True
                    l.Visible = True
                Case 13 : a.Visible = True
                    b.Visible = True
                    c.Visible = True
                    d.Visible = True
                    e.Visible = True
                    f.Visible = True
                    g.Visible = True
                    h.Visible = True
                    i.Visible = True
                    j.Visible = True
                    k.Visible = True
                    l.Visible = True
                    m.Visible = True
                Case 14 : a.Visible = True
                    b.Visible = True
                    c.Visible = True
                    d.Visible = True
                    e.Visible = True
                    f.Visible = True
                    g.Visible = True
                    h.Visible = True
                    i.Visible = True
                    j.Visible = True
                    k.Visible = True
                    l.Visible = True
                    m.Visible = True
                    n.Visible = True
                Case 15 : a.Visible = True
                    b.Visible = True
                    c.Visible = True
                    d.Visible = True
                    e.Visible = True
                    f.Visible = True
                    g.Visible = True
                    h.Visible = True
                    i.Visible = True
                    j.Visible = True
                    k.Visible = True
                    l.Visible = True
                    m.Visible = True
                    n.Visible = True
                    o.Visible = True
                Case 16 : a.Visible = True
                    b.Visible = True
                    c.Visible = True
                    d.Visible = True
                    e.Visible = True
                    f.Visible = True
                    g.Visible = True
                    h.Visible = True
                    i.Visible = True
                    j.Visible = True
                    k.Visible = True
                    l.Visible = True
                    m.Visible = True
                    n.Visible = True
                    o.Visible = True
                    p.Visible = True

            End Select
        End If
    End Function

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CarATimer.Tick

        'traffic light
        Dim track As Integer
        track = TimeAllocate_W.Text
        If track = 0 Then
            OvalShape1.FillColor = Color.Red
            OvalShape2.FillColor = Color.White
        ElseIf track <= 120 Then
            OvalShape2.FillColor = Color.Green
            OvalShape1.FillColor = Color.White
        End If
        If track <= 20 And OvalShape8.FillColor = Color.White And OvalShape9.FillColor = Color.White Then

            OvalShape7.FillColor = Color.Orange
        End If
        If control = 0 Then

            CarBTimer.Start()
            TimeInB.Start()
        End If

        'for Cars going forward in the section A of the Road
        ChPositionA1(car1, control)
        ChPositionA1(Car3, control)
        ChPositionA1(Car5, control)
        ChPositionA1(Car7, control)
        ChPositionA1(car9, control)
        ChPositionA1(Car11, control)
        ChPositionA1(Car13, control)
        ChPositionA1(car15, control)
        'for Cars going down in section  A of the road
        ChPositionA2(car2, PictureBox49, go1)
        ChPositionA2(Car4, PictureBox49, go2)
        ChPositionA2(car6, PictureBox49, go8)
        ChPositionA2(Car8, PictureBox49, go3)
        ChPositionA2(car10, PictureBox49, go4)
        ChPositionA2(car12, PictureBox49, go5)
        ChPositionA2(car14, PictureBox49, go6)
        ChPositionA2(car16, PictureBox49, go7)

    End Sub

    Private Sub Timer2_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CarBTimer.Tick

        'traffic light
        Dim track1 As Integer
        track1 = TimeAllocate_N.Text
        If track1 = 0 Then
            OvalShape9.FillColor = Color.Red
            OvalShape8.FillColor = Color.White
            OvalShape7.FillColor = Color.White
        ElseIf track1 <= 120 Then
            OvalShape8.FillColor = Color.Green
            OvalShape9.FillColor = Color.White
            OvalShape7.FillColor = Color.White
        End If
        If track1 <= 20 And OvalShape5.FillColor = Color.White And OvalShape6.FillColor = Color.White Then
            OvalShape4.FillColor = Color.Orange
        End If
        If control1 = 0 Then

            CarCTimer.Start()
            TimerInC.Start()
        End If
        'for car going left in section B
        ChPositionB1(Car17)
        ChPositionB1(Car19)
        ChPositionB1(Car21)
        ChPositionB1(Car23)
        ChPositionB1(car25)
        ChPositionB1(Car27)
        ChPositionB1(Car29)
        ChPositionB1(car31)

        'for Cars going down in section  B of the road
        ChPositionB2(Car18, PictureBox49, go9, control1)
        ChPositionB2(Car20, PictureBox49, go10, control1)
        ChPositionB2(Car22, PictureBox49, go11, control1)
        ChPositionB2(Car24, PictureBox49, go12, control1)
        ChPositionB2(Car26, PictureBox49, go13, control1)
        ChPositionB2(Car28, PictureBox49, go14, control1)
        ChPositionB2(Car30, PictureBox49, go15, control1)
        ChPositionB2(Car32, PictureBox49, go16, control1)


    End Sub

    Private Sub Timer3_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TimeInA.Tick

        control = TimeAllocate_W.Text
        If control = 0 Then
            control = 0
        Else
            control -= 1
        End If
        TimeAllocate_W.Text = control.ToString
    End Sub

    Private Sub Timer4_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TimeInB.Tick
        control1 = TimeAllocate_N.Text
        If TimeAllocate_W.Text = 0 Then
            control1 = TimeAllocate_N.Text
            If control1 = 0 Then
                control1 = 0

            Else
                control1 -= 1
                TimeAllocate_N.Text = control1.ToString
            End If

        End If

    End Sub

    Private Sub CarCTimer_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CarCTimer.Tick
        Dim track3 As Integer
        track3 = TimeAllocate_S.Text
        If track3 = 0 Then
            OvalShape5.FillColor = Color.White
            OvalShape6.FillColor = Color.Red
            OvalShape4.FillColor = Color.White
        ElseIf track3 <= 120 Then
            OvalShape6.FillColor = Color.White
            OvalShape5.FillColor = Color.Green
            OvalShape4.FillColor = Color.White
        End If
        If track3 <= 20 And OvalShape1.FillColor = Color.White Then
            OvalShape1.FillColor = Color.White
            OvalShape3.FillColor = Color.Orange
        End If



        'for Cars going right in section  C of the road

        ChPositionC1(Car33, car1, go17)
        ChPositionC1(car35, car1, go18)
        ChPositionC1(car37, car1, go19)
        ChPositionC1(car39, car1, go20)
        ChPositionC1(car41, car1, go21)
        ChPositionC1(car43, car1, go22)
        ChPositionC1(car45, car1, go23)
        ChPositionC1(car47, car1, go24)
        'for Cars going letf in section  C of the road
        ChPositionC2(car34, PictureBox1, go25, control2)
        ChPositionC2(car36, PictureBox1, go26, control2)
        ChPositionC2(car38, PictureBox1, go27, control2)
        ChPositionC2(car40, PictureBox1, go28, control2)
        ChPositionC2(car42, PictureBox1, go29, control2)
        ChPositionC2(car44, PictureBox1, go30, control2)
        ChPositionC2(car46, PictureBox1, go31, control2)
        ChPositionC2(car48, PictureBox1, go32, control2)

    End Sub

    Private Sub RectangleShape1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RectangleShape1.Click, RectangleShape3.Click, RectangleShape4.Click

    End Sub

    Private Sub TimerInC_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TimerInC.Tick
        control2 = TimeAllocate_S.Text
        If TimeAllocate_N.Text = 0 Then
            control2 = TimeAllocate_S.Text
            If control2 = 0 Then
                control2 = 0

            Else
                control2 -= 1
                TimeAllocate_S.Text = control2.ToString
            End If

        End If

    End Sub

    Private Sub OvalShape8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OvalShape8.Click

    End Sub


    Private Sub TextAnimation_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs)




    End Sub

    Private Sub TextAnimation2_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Label10.Visible = False
    End Sub

    Private Sub Switch(ByVal a As Integer)
        Throw New NotImplementedException
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Dim a, b, c As Integer
        a = CarsWaiting_W.Text
        b = TimeWaiting_W.Text
        TimeAllocate(a, b, c, TimeAllocate_W)

        LoadCars(CarsWaiting_W.Text, car1, car2, Car3, Car4, Car5, car6, Car7, Car8, car9, car10, Car11, car12, Car13, car14, car15, car16)
        Button4.Enabled = True
    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
       
        Dim a1, b2, c3 As Integer
        a1 = CarsWaiting_N.Text
        b2 = Timewaiting_N.Text
        TimeAllocate(a1, b2, c3, TimeAllocate_N)
        LoadCars(CarsWaiting_N.Text, Car17, Car18, Car19, Car20, Car21, Car22, Car23, Car24, car25, Car26, Car27, Car28, Car29, Car30, car31, Car32)
        Button3.Enabled = True
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        Dim a3, b3, c3 As Integer
        a3 = CarWaiting_S.Text
        b3 = TimeWaiting_S.Text
        TimeAllocate(a3, b3, c3, TimeAllocate_S)
        LoadCars(CarWaiting_S.Text, Car33, car34, car35, car36, car37, car38, car39, car40, car41, car42, car43, car44, car45, car46, car47, car48)
        Simulate.Enabled = True
    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        My.Computer.Audio.Play(My.Resources.Play, AudioPlayMode.Background)
    End Sub

    Private Sub ProgressBar1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub
End Class
