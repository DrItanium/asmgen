/*
asmgen
Copyright (c) 2025, Joshua Scoggins
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef I960_ASMGEN_H__
#define I960_ASMGEN_H__
#include <string>
#include <iostream>
namespace i960 {
struct Statement {
    virtual void emit(std::ostream& stream) const = 0;
};
struct Label : public Statement {
    std::string name;
    std::string comment;
    void emit(std::ostream& stream) const override {
        stream << name << ": ";
        if (!comment.empty()) {
            stream << " # " << comment;
        }
        stream << std::endl;
    }
};
struct Instruction : public Statement {
    std::string name;
    std::string src1;
    std::string src2;
    std::string srcDest;
    std::string comment;
    void emit(std::ostream& stream) const override {
        stream << name << "\t";
        if (!src1.empty()) {
            stream << src1;
            if (!src2.empty()) {
                stream << ", " << src2;
                if (!srcDest.empty()) {
                    stream << ", " << srcDest;
                }
            }
        } else if (!src2.empty()) {
            stream << src2;
            if (!srcDest.empty()) {
                stream << ", " << srcDest;
            }
        } else if (!srcDest.empty()) {
            stream << srcDest;
        }

        if (!comment.empty()) {
            stream << " # " << comment;
        }
        stream << std::endl;
    }
};
}
#endif // end !defined(I960_ASMGEN_H__)
