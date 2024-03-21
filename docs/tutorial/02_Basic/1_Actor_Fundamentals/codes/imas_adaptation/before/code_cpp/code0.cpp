#include <iostream>
#include <string>
#include <memory>

std::unique_ptr<std::string> greeting()
{
    return std::unique_ptr<std::string>(new std::string("Good Morning from pure C++"));
}

int main(int argc, char* argv[])
{
    std::cout << *greeting() << std::endl;
}