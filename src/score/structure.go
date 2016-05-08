package score

type JudgingId int64
type SubmissionId int64
type TeamId int64
type ProblemId int64

type Judging struct {
    Id JudgingId
    Time float64
    Submission SubmissionId
    Outcome string
}

type Submission struct {
    Id SubmissionId
    Time float64
    Team TeamId
    Problem ProblemId
}
type Submissions []Submission

func (s Submissions) Len() int {
    return len(s)
}
func (s Submissions) Less(i, j int) bool {
    return s[i].Id < s[j].Id
}
func (s Submissions) Swap(i, j int) {
    s[i], s[j] = s[j], s[i]
}

type Contest struct {
    Id int64
    Name string
    Freeze int64
    Start int64
    End int64
    Unfreeze int64
    Penalty int64
}

type ContestConfig struct {
    Compile_penalty int64
}

type Team struct {
    Id TeamId
    Name string
    Affiliation string
}
