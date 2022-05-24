Imports System.Math
Public Class Form1
    Dim inputM(5, 5), rM(5, 5), prodM(5, 5) As Double
    Dim Q(5, 5), Qn(5, 5) As Double
    Dim QT(5, 5), prodQQT(5, 5) As Double
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Eigen.Click
        Initialize()
        iterate()
        Solution()
    End Sub
    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Random.Click
        'Generates random symmetric matrix entries [-9,9]
        For i = 0 To 4
            For j = 0 To 4
                inputM(i, j) = Int(Rnd() * 18 - 9)
                inputM(j, i) = inputM(i, j)
            Next
        Next

        TextBox1.Text = inputM(0, 0)
        TextBox2.Text = inputM(0, 1)
        TextBox3.Text = inputM(0, 2)
        TextBox4.Text = inputM(0, 3)
        TextBox5.Text = inputM(0, 4)
        TextBox6.Text = inputM(1, 0)
        TextBox7.Text = inputM(1, 1)
        TextBox8.Text = inputM(1, 2)
        TextBox9.Text = inputM(1, 3)
        TextBox10.Text = inputM(1, 4)
        TextBox11.Text = inputM(2, 0)
        TextBox12.Text = inputM(2, 1)
        TextBox13.Text = inputM(2, 2)
        TextBox14.Text = inputM(2, 3)
        TextBox15.Text = inputM(2, 4)
        TextBox16.Text = inputM(3, 0)
        TextBox17.Text = inputM(3, 1)
        TextBox18.Text = inputM(3, 2)
        TextBox19.Text = inputM(3, 3)
        TextBox20.Text = inputM(3, 4)
        TextBox21.Text = inputM(4, 0)
        TextBox22.Text = inputM(4, 1)
        TextBox23.Text = inputM(4, 2)
        TextBox24.Text = inputM(4, 3)
        TextBox25.Text = inputM(4, 4)


    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Gram.Click
        'Shows the first Q R factors of the entered matrix
        Initialize()
        GramSchmidt()

        Dim m As Double
        m = 0
        For i = 0 To 4                              'Matrix multiplication i,j columns and rows k place holder
            m = 0
            For j = 0 To 4
                m = 0
                For k = 0 To 4
                    m = m + Q(j, k) * rM(k, i)
                    prodM(j, i) = m
                Next
            Next
        Next


        Matrix1.Text = ""
        Matrix2.Text = ""
        Matrix3.Text = ""
        Dim output1, output2, output3 As String
        output1 = ""
        output2 = ""
        output3 = ""
        For i = 0 To 4
            For j = 0 To 4
                output1 = output1 & vbTab & Round(Q(i, j), 2)
                output2 = output2 & vbTab & Round(rM(i, j), 2)
                output3 = output3 & vbTab & Round(prodM(i, j), 2)
            Next
            output1 = output1 & vbCrLf
            output2 = output2 & vbCrLf
            output3 = output3 & vbCrLf
        Next

        Matrix1.Text = output1
        Matrix2.Text = output2
        Matrix3.Text = output3


        Title1.Visible = True
        Title2.Visible = True
        Title3.Visible = True
        Title1.Text = "Q"
        Title2.Text = "R"
        Title3.Text = "Product"
        Matrix3.Visible = True
        GeneralSolution.Visible = False
        equal.Visible = False
        c1.Visible = False
        c2.Visible = False
        c3.Visible = False
        c4.Visible = False
        c5.Visible = False
        v1.Visible = False
        v2.Visible = False
        v3.Visible = False
        v4.Visible = False
        v5.Visible = False
        e1.Visible = False
        e2.Visible = False
        e3.Visible = False
        e4.Visible = False
        e5.Visible = False
        lambda1.Visible = False
        lambda2.Visible = False
        lambda3.Visible = False
        lambda4.Visible = False
        lambda5.Visible = False
        plus1.Visible = False
        plus2.Visible = False
        plus3.Visible = False
        plus4.Visible = False
    End Sub

    Private Sub QQT_Click(sender As Object, e As EventArgs) Handles QQT.Click

        Initialize()
        GramSchmidt()

        For i = 0 To 4                      'Transpose Q (technically not neccessary since Q is symmetric)
            For j = 0 To 4
                QT(i, j) = Q(j, i)
            Next
        Next

        Dim m As Double
        m = 0
        For i = 0 To 4
            m = 0
            For j = 0 To 4                      'Multiply Q and QT
                m = 0
                For k = 0 To 4
                    m = m + Q(j, k) * QT(k, i)
                    prodQQT(j, i) = m
                Next
            Next
        Next

        Matrix1.Text = ""
        Matrix2.Text = ""
        Matrix3.Text = ""
        Dim output1, output2, output3 As String
        output1 = ""
        output2 = ""
        output3 = ""
        For i = 0 To 4
            For j = 0 To 4
                output1 = output1 & vbTab & Round(Q(i, j), 2)
                output2 = output2 & vbTab & Round(QT(i, j), 2)
                output3 = output3 & vbTab & Round(prodQQT(i, j), 2)
            Next
            output1 = output1 & vbCrLf
            output2 = output2 & vbCrLf
            output3 = output3 & vbCrLf
        Next

        Matrix1.Text = output1
        Matrix2.Text = output2
        Matrix3.Text = output3

        Title1.Visible = True
        Title2.Visible = True
        Title3.Visible = True
        Title1.Text = "Q"
        Title2.Text = "Transpose of Q (Q^T)"
        Title3.Text = "Q*Q^T=I"
        Matrix3.Visible = True
        GeneralSolution.Visible = False
        equal.Visible = False
        c1.Visible = False
        c2.Visible = False
        c3.Visible = False
        c4.Visible = False
        c5.Visible = False
        v1.Visible = False
        v2.Visible = False
        v3.Visible = False
        v4.Visible = False
        v5.Visible = False
        e1.Visible = False
        e2.Visible = False
        e3.Visible = False
        e4.Visible = False
        e5.Visible = False
        lambda1.Visible = False
        lambda2.Visible = False
        lambda3.Visible = False
        lambda4.Visible = False
        lambda5.Visible = False
        plus1.Visible = False
        plus2.Visible = False
        plus3.Visible = False
        plus4.Visible = False
    End Sub

    Private Sub Instructions_Click(sender As Object, e As EventArgs) Handles Instructions.Click
        Form2.Show()
    End Sub

    Private Sub Quit_Click(sender As Object, e As EventArgs) Handles Quit.Click
        End
    End Sub

    Private Sub Initialize()

        For i = 0 To 4
            For j = 0 To 4
                Q(i, j) = 0
                Qn(i, j) = 0
                prodM(i, j) = 0
                rM(i, j) = 0
            Next
        Next

        inputM(0, 0) = TextBox1.Text
        inputM(0, 1) = TextBox2.Text
        inputM(0, 2) = TextBox3.Text
        inputM(0, 3) = TextBox4.Text
        inputM(0, 4) = TextBox5.Text
        inputM(1, 0) = TextBox6.Text
        inputM(1, 1) = TextBox7.Text
        inputM(1, 2) = TextBox8.Text
        inputM(1, 3) = TextBox9.Text
        inputM(1, 4) = TextBox10.Text
        inputM(2, 0) = TextBox11.Text
        inputM(2, 1) = TextBox12.Text
        inputM(2, 2) = TextBox13.Text
        inputM(2, 3) = TextBox14.Text
        inputM(2, 4) = TextBox15.Text
        inputM(3, 0) = TextBox16.Text
        inputM(3, 1) = TextBox17.Text
        inputM(3, 2) = TextBox18.Text
        inputM(3, 3) = TextBox19.Text
        inputM(3, 4) = TextBox20.Text
        inputM(4, 0) = TextBox21.Text
        inputM(4, 1) = TextBox22.Text
        inputM(4, 2) = TextBox23.Text
        inputM(4, 3) = TextBox24.Text
        inputM(4, 4) = TextBox25.Text

    End Sub

    Sub Factor()
        Dim x, a, b, c, d, e, v1, v2, v3, v4, vA, vB, vC, p1, p2, pA As Double

        'modified gram-schmidt
        For i = 0 To 4
            For j = 0 To 4                          'a through e are inner product constants
                Q(i, j) = inputM(i, j)              'v1 through vC and p1 through pA intial vectors
            Next
        Next

        For i = 0 To 4
            a = a + Q(i, 0) * Q(i, 0)
        Next
        If a <> 0 Then
            For i = 0 To 4
                Q(i, 0) = Q(i, 0) / Sqrt(a)         'first Orthonormal vector
            Next
        End If

        For i = 0 To 4
            v1 = v1 + Q(i, 0) * Q(i, 1)
            v2 = v2 + Q(i, 0) * Q(i, 2)
            v3 = v3 + Q(i, 0) * Q(i, 3)
            v4 = v4 + Q(i, 0) * Q(i, 4)
        Next

        For i = 0 To 4
            Q(i, 1) = Q(i, 1) - v1 * Q(i, 0)
            Q(i, 2) = Q(i, 2) - v2 * Q(i, 0)
            Q(i, 3) = Q(i, 3) - v3 * Q(i, 0)
            Q(i, 4) = Q(i, 4) - v4 * Q(i, 0)
            b = b + Q(i, 1) * Q(i, 1)
        Next
        If b <> 0 Then
            For i = 0 To 4
                Q(i, 1) = Q(i, 1) / Sqrt(b)     'second Orthonormal vector
            Next
        End If

        For i = 0 To 4
            vA = vA + Q(i, 1) * Q(i, 2)
            vB = vB + Q(i, 1) * Q(i, 3)
            vC = vC + Q(i, 1) * Q(i, 4)
        Next

        For i = 0 To 4
            Q(i, 2) = Q(i, 2) - vA * Q(i, 1)
            Q(i, 3) = Q(i, 3) - vB * Q(i, 1)
            Q(i, 4) = Q(i, 4) - vC * Q(i, 1)
            c = c + Q(i, 2) * Q(i, 2)
        Next
        If c <> 0 Then
            For i = 0 To 4
                Q(i, 2) = Q(i, 2) / Sqrt(c)    'third Orthonormal vector
            Next
        End If
        For i = 0 To 4
            p1 = p1 + Q(i, 2) * Q(i, 3)
            p2 = p2 + Q(i, 2) * Q(i, 4)
        Next

        For i = 0 To 4
            Q(i, 3) = Q(i, 3) - p1 * Q(i, 2)
            Q(i, 4) = Q(i, 4) - p2 * Q(i, 2)
            d = d + Q(i, 3) * Q(i, 3)
        Next
        If d <> 0 Then
            For i = 0 To 4
                Q(i, 3) = Q(i, 3) / Sqrt(d) 'fourth orthonormal vector
            Next
        End If

        For i = 0 To 4
            pA = pA + Q(i, 3) * Q(i, 4)
        Next

        For i = 0 To 4
            Q(i, 4) = Q(i, 4) - pA * Q(i, 3)
            e = e + Q(i, 4) * Q(i, 4)
        Next
        If e <> 0 Then
            For i = 0 To 4
                Q(i, 4) = Q(i, 4) / Sqrt(e) 'fifth orthonormal vector
            Next
        End If

        For i = 0 To 4
            For j = 0 To 4
                x = 0                                          'R matrix generation
                For k = 0 To 4                                  'Dot product Q and inputm 
                    x = x + Q(k, j) * inputM(k, i)
                    rM(j, i) = x
                Next
            Next
        Next

    End Sub

    Private Sub GramSchmidt()

        Dim x, a, b, c, d, v1, v2, v3, v4, v5, v6, v7, v8 As Double

        For i = 0 To 4
            For j = 0 To 4                      'initialize Q as input
                Q(i, j) = inputM(i, j)
            Next
        Next

        For i = 0 To 4
            v1 = v1 + Q(i, 0) * Q(i, 0)             'a through d are inner product constants
        Next

        For i = 0 To 4
            If Sqrt(v1) = 0 Then                    'All if statements to prevent NaN outputs (dividing by zero)
                Q(i, 0) = 0
            Else
                Q(i, 0) = Q(i, 0) / Sqrt(v1)
            End If
        Next
        v1 = 0

        For i = 0 To 4
            v1 = v1 + Q(i, 0) * Q(i, 1)
            v2 = v2 + Q(i, 0) * Q(i, 0)
        Next

        If v2 <> 0 Then
            For i = 0 To 4
                Q(i, 1) = Q(i, 1) - (v1 / v2) * Q(i, 0)
            Next
        End If

        For i = 0 To 4
            a = a + Q(i, 1) * Q(i, 1)
        Next
        If Sqrt(a) <> 0 Then
            For i = 0 To 4
                Q(i, 1) = Q(i, 1) / Sqrt(a)
            Next
        End If
        v1 = 0
        v2 = 0

        For i = 0 To 4
            v1 = v1 + Q(i, 0) * Q(i, 2)
            v2 = v2 + Q(i, 0) * Q(i, 0)
            v3 = v3 + Q(i, 1) * Q(i, 2)
            v4 = v4 + Q(i, 1) * Q(i, 1)
        Next
        For i = 0 To 4
            If v2 = 0 And v4 = 0 Then
                Q(i, 2) = Q(i, 2)
            ElseIf v2 = 0 Then
                Q(i, 2) = Q(i, 2) - (v3 / v4) * Q(i, 1)
            ElseIf v4 = 0 Then
                Q(i, 2) = Q(i, 2) - (v1 / v2) * Q(i, 0)
            Else
                Q(i, 2) = Q(i, 2) - (v1 / v2) * Q(i, 0) - (v3 / v4) * Q(i, 1)
            End If
        Next

        For i = 0 To 4
            b = b + Q(i, 2) * Q(i, 2)
        Next
        If Sqrt(b) <> 0 Then
            For i = 0 To 4
                Q(i, 2) = Q(i, 2) / Sqrt(b)
            Next
        End If

        v1 = 0
        v2 = 0
        v3 = 0
        v4 = 0

        For i = 0 To 4
            v1 = v1 + Q(i, 0) * Q(i, 3)
            v2 = v2 + Q(i, 0) * Q(i, 0)
            v3 = v3 + Q(i, 1) * Q(i, 3)
            v4 = v4 + Q(i, 1) * Q(i, 1)
            v5 = v5 + Q(i, 2) * Q(i, 3)
            v6 = v6 + Q(i, 2) * Q(i, 2)
        Next

        For i = 0 To 4
            If v2 = 0 And v4 = 0 And v6 = 0 Then                '2^3 if statements 3 possible zeros
                Q(i, 3) = Q(i, 3)
            ElseIf v2 = 0 And v4 = 0 Then
                Q(i, 3) = Q(i, 3) - (v5 / v6) * Q(i, 2)
            ElseIf v2 = 0 And v6 = 0 Then
                Q(i, 3) = Q(i, 3) - (v3 / v4) * Q(i, 1)
            ElseIf v4 = 0 And v6 = 0 Then
                Q(i, 3) = Q(i, 3) - (v1 / v2) * Q(i, 0)
            ElseIf v2 = 0 Then
                Q(i, 3) = Q(i, 3) - (v3 / v4) * Q(i, 1) - (v5 / v6) * Q(i, 2)
            ElseIf v4 = 0 Then
                Q(i, 3) = Q(i, 3) - (v1 / v2) * Q(i, 0) - (v5 / v6) * Q(i, 2)
            ElseIf v6 = 0 Then
                Q(i, 3) = Q(i, 3) - (v1 / v2) * Q(i, 0) - (v3 / v4) * Q(i, 1)
            Else
                Q(i, 3) = Q(i, 3) - (v1 / v2) * Q(i, 0) - (v3 / v4) * Q(i, 1) - (v5 / v6) * Q(i, 2)
            End If
        Next

        For i = 0 To 4
            c = c + Q(i, 3) * Q(i, 3)
        Next
        For i = 0 To 4
            Q(i, 3) = Q(i, 3) / Sqrt(c)
        Next

        v1 = 0
        v2 = 0
        v3 = 0
        v4 = 0
        v5 = 0
        v6 = 0

        For i = 0 To 4
            v1 = v1 + Q(i, 0) * Q(i, 4)
            v2 = v2 + Q(i, 0) * Q(i, 0)
            v3 = v3 + Q(i, 1) * Q(i, 4)
            v4 = v4 + Q(i, 1) * Q(i, 1)
            v5 = v5 + Q(i, 2) * Q(i, 4)
            v6 = v6 + Q(i, 2) * Q(i, 2)
            v7 = v7 + Q(i, 3) * Q(i, 4)
            v8 = v8 + Q(i, 3) * Q(i, 3)
        Next

        For i = 0 To 4
            If v2 = 0 And v4 = 0 And v6 = 0 And v8 = 0 Then   '2^4=16 if statements or sum 4Ci (i=0 to 4) four possible zeros
                Q(i, 4) = Q(i, 4)
            ElseIf v2 = 0 And v4 = 0 And v6 = 0 Then
                Q(i, 4) = Q(i, 4) - (v7 / v8) * Q(i, 3)
            ElseIf v2 = 0 And v4 = 0 And v8 = 0 Then
                Q(i, 4) = Q(i, 4) - (v5 / v6) * Q(i, 2)
            ElseIf v2 = 0 And v6 = 0 And v8 = 0 Then
                Q(i, 4) = Q(i, 4) - (v3 / v4) * Q(i, 1)
            ElseIf v4 = 0 And v6 = 0 And v8 = 0 Then
                Q(i, 4) = Q(i, 4) - (v1 / v2) * Q(i, 0)
            ElseIf v2 = 0 And v4 = 0 Then
                Q(i, 4) = Q(i, 4) - (v5 / v6) * Q(i, 2) - (v7 / v8) * Q(i, 3)
            ElseIf v2 = 0 And v6 = 0 Then
                Q(i, 4) = Q(i, 4) - (v3 / v4) * Q(i, 1) - (v7 / v8) * Q(i, 3)
            ElseIf v2 = 0 And v8 = 0 Then
                Q(i, 4) = Q(i, 4) - (v3 / v4) * Q(i, 1) - (v5 / v6) * Q(i, 2)
            ElseIf v4 = 0 And v6 = 0 Then
                Q(i, 4) = Q(i, 4) - (v1 / v2) * Q(i, 0) - (v7 / v8) * Q(i, 3)
            ElseIf v4 = 0 And v8 = 0 Then
                Q(i, 4) = Q(i, 4) - (v1 / v2) * Q(i, 0) - (v5 / v6) * Q(i, 2)
            ElseIf v6 = 0 And v8 = 0 Then
                Q(i, 4) = Q(i, 4) - (v1 / v2) * Q(i, 0) - (v3 / v4) * Q(i, 1)
            ElseIf v2 = 0 Then
                Q(i, 4) = Q(i, 4) - (v3 / v4) * Q(i, 1) - (v5 / v6) * Q(i, 2) - (v7 / v8) * Q(i, 3)
            ElseIf v4 = 0 Then
                Q(i, 4) = Q(i, 4) - (v1 / v2) * Q(i, 0) - (v5 / v6) * Q(i, 2) - (v7 / v8) * Q(i, 3)
            ElseIf v6 = 0 Then
                Q(i, 4) = Q(i, 4) - (v1 / v2) * Q(i, 0) - (v3 / v4) * Q(i, 1) - (v7 / v8) * Q(i, 3)
            ElseIf v8 = 0 Then
                Q(i, 4) = Q(i, 4) - (v1 / v2) * Q(i, 0) - (v3 / v4) * Q(i, 1) - (v5 / v6) * Q(i, 2)
            Else
                Q(i, 4) = Q(i, 4) - (v1 / v2) * Q(i, 0) - (v3 / v4) * Q(i, 1) - (v5 / v6) * Q(i, 2) - (v7 / v8) * Q(i, 3)
            End If
        Next

        For i = 0 To 4
            d = d + Q(i, 4) * Q(i, 4)
        Next
        If Sqrt(d) <> 0 Then
            For i = 0 To 4
                Q(i, 4) = Q(i, 4) / Sqrt(d)
            Next
        End If

        For i = 0 To 4
            For j = 0 To 4
                x = 0                                          'R matrix generation
                For k = 0 To 4
                    x = x + Q(k, j) * inputM(k, i)
                    rM(j, i) = x
                Next
            Next
        Next

    End Sub

    Sub iterate()
        Dim m, m2 As Double

        For i = 0 To 4
            Qn(i, i) = 1        ' let Qn initially be identity matrix
        Next


        For h = 1 To 500

            Factor()

            For i = 0 To 4
                m = 0
                For j = 0 To 4
                    m = 0                       'multiply R and Q then let product= An
                    For k = 0 To 4
                        m = m + rM(j, k) * Q(k, i)
                        inputM(j, i) = m
                    Next
                Next
            Next

            For i = 0 To 4
                m2 = 0
                For j = 0 To 4                          'Product of Q matrices
                    m2 = 0                                 'Eigenvector matrix Sn= Q1*Q2..*Qn 
                    For k = 0 To 4
                        m2 = m2 + Qn(j, k) * Q(k, i)
                        prodM(j, i) = m2
                    Next
                Next
            Next

            For i = 0 To 4                      'Qn= previous product
                For j = 0 To 4
                    Qn(i, j) = prodM(i, j)
                Next
            Next

        Next

        Dim output, output2 As String
        output = ""
        output2 = ""
        For i = 0 To 4
            For j = 0 To 4
                output = output & vbTab & Round(inputM(i, j), 4)
                output2 = output2 & vbTab & Round(prodM(i, j), 4)
            Next
            output = output & vbCrLf
            output2 = output2 & vbCrLf
        Next

        Matrix2.Text = output2
        Matrix1.Text = output


    End Sub

    Sub Solution()
        lambda1.Text = Round(inputM(0, 0), 2) & "  " & "t"
        lambda2.Text = Round(inputM(1, 1), 2) & "  " & "t"
        lambda3.Text = Round(inputM(2, 2), 2) & "  " & "t"
        lambda4.Text = Round(inputM(3, 3), 2) & "  " & "t"
        lambda5.Text = Round(inputM(4, 4), 2) & "  " & "t"

        v1.Text = Round(prodM(0, 0), 2) & vbCrLf & Round(prodM(1, 0), 2) & vbCrLf & Round(prodM(2, 0), 2) & vbCrLf & Round(prodM(3, 0), 2) & vbCrLf & Round(prodM(4, 0), 2)
        v2.Text = Round(prodM(0, 1), 2) & vbCrLf & Round(prodM(1, 1), 2) & vbCrLf & Round(prodM(2, 1), 2) & vbCrLf & Round(prodM(3, 1), 2) & vbCrLf & Round(prodM(4, 1), 2)
        v3.Text = Round(prodM(0, 2), 2) & vbCrLf & Round(prodM(1, 2), 2) & vbCrLf & Round(prodM(2, 2), 2) & vbCrLf & Round(prodM(3, 2), 2) & vbCrLf & Round(prodM(4, 2), 2)
        v4.Text = Round(prodM(0, 3), 2) & vbCrLf & Round(prodM(1, 3), 2) & vbCrLf & Round(prodM(2, 3), 2) & vbCrLf & Round(prodM(3, 3), 2) & vbCrLf & Round(prodM(4, 3), 2)
        v5.Text = Round(prodM(0, 4), 2) & vbCrLf & Round(prodM(1, 4), 2) & vbCrLf & Round(prodM(2, 4), 2) & vbCrLf & Round(prodM(3, 4), 2) & vbCrLf & Round(prodM(4, 4), 2)

        Title1.Visible = True
        Title2.Visible = True
        Title3.Visible = False
        Matrix3.Visible = False
        Title1.Text = "Eigenvalues"
        Title2.Text = "Eigenvectors"

        GeneralSolution.Visible = True
        equal.Visible = True
        c1.Visible = True
        c2.Visible = True
        c3.Visible = True
        c4.Visible = True
        c5.Visible = True
        v1.Visible = True
        v2.Visible = True
        v3.Visible = True
        v4.Visible = True
        v5.Visible = True
        e1.Visible = True
        e2.Visible = True
        e3.Visible = True
        e4.Visible = True
        e5.Visible = True
        lambda1.Visible = True
        lambda2.Visible = True
        lambda3.Visible = True
        lambda4.Visible = True
        lambda5.Visible = True
        plus1.Visible = True
        plus2.Visible = True
        plus3.Visible = True
        plus4.Visible = True

    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        TextBox1.Text = 1
        TextBox2.Text = 1
        TextBox3.Text = 2
        TextBox4.Text = 3
        TextBox5.Text = 4
        TextBox6.Text = 1
        TextBox7.Text = 2
        TextBox8.Text = 1
        TextBox9.Text = 2
        TextBox10.Text = 1
        TextBox11.Text = 2
        TextBox12.Text = 1
        TextBox13.Text = 3
        TextBox14.Text = 2
        TextBox15.Text = 1
        TextBox16.Text = 3
        TextBox17.Text = 2
        TextBox18.Text = 2
        TextBox19.Text = 4
        TextBox20.Text = 3
        TextBox21.Text = 4
        TextBox22.Text = 1
        TextBox23.Text = 1
        TextBox24.Text = 3
        TextBox25.Text = 5


    End Sub
End Class
