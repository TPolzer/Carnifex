/*
 * This file is part of "The Scoreboard"
 * Copyright (C) 2016  Tobias Polzer, Dominik Paulus

 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */
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
	SimulatedStart *int64
	SimulationSpeed *float64
    End int64
    Unfreeze int64
    Penalty int64
}

type ContestConfig struct {
    Compile_penalty int
}

type Team struct {
    Id TeamId
    Name string
    Affiliation string
}
