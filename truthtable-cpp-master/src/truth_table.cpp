/*******************
** 08-08-2021
** BY Yasar Arafat E
*/
#include <iostream>
#include <string>
#include <vector>
#include <set>
#include <map>
#include <bitset>
#include <stack>
#include <algorithm>
#include <iterator>
#include <cstdlib>
#include <cctype>
#include <cmath>
using namespace std;
int isoperator(char c)
{
    switch (c)
    {
    case '!':
    case '&':
    case '|':
    case '>':
    case '#':
        return 1;
    }
    return 0;
}
int isbinaryoperator(char c)
{
    return c != '!' && isoperator(c);
}
int isinvalidchar(char c)
{
    if (c == '(' || c == ')')
        return 0;
    else if (isupper(c) || isoperator(c))
        return 0;
    return 1;
}
int isupperalpha(char c)
{
    return isupper(c);
}
int priority(char opr)
{
    switch (opr)
    {
    case '!':
        return 5;
    case '&':
        return 4;
    case '|':
        return 3;
    case '>':
        return 2;
    case '#':
        return 1;
    default:
        return 0;
    }
}
int is_valid_expression(const string& exp)
{
    if (exp.empty())
    {
        cout << "You didn't enter anything!" << endl;
        return 0;
    }
    
    string::const_iterator pos;
    pos = find_if(exp.begin(), exp.end(), isinvalidchar);
    if (pos != exp.end())
    {
        cout << "Invalid characters: '" << *pos << '\'' << endl;
        return 0;
    }
    
    if (!isupper(*(exp.begin())) && *(exp.begin()) != '(' && *(exp.begin()) != '!')
    {
        cout << "The beginning character is wrong!" << endl;
        return 0;
    }
    if (!isupper(*(exp.rbegin())) && *(exp.rbegin()) != ')')
    {
        cout << "The end character is wrong!" << endl;
        return 0;
    }
    
    if (0 == count_if(exp.begin(), exp.end(), isupperalpha))
    {
        cout << "No meta!" << endl;
        return 0;
    }
    
    if (count(exp.begin(), exp.end(), '(') != count(exp.begin(), exp.end(), ')'))
    {
        cout << "The number of parentheses varies!" << endl;
        return 0;
    }
    
    vector<int> bracketpos[2];
    for (pos = exp.begin(); pos != exp.end(); ++pos)
    {
        if (*pos == '(')
            bracketpos[0].push_back(distance(exp.begin(), pos));
        else if (*pos == ')')
            bracketpos[1].push_back(distance(exp.begin(), pos));
    }
    vector<int>::size_type v;
    for (v = 0; v != bracketpos[0].size(); ++v)
    {
        if (bracketpos[0][v] > bracketpos[1][v])
        {
            cout << "Bracket positions don't match!" << endl;
            return 0;
        }
    }
    
    string::size_type i;
    for (i = 0; i != exp.size() - 1; ++i)
    {
        if (isupper(exp[i]))
        {
            if (exp[i + 1] != ')' && !isbinaryoperator(exp[i + 1]))
            {
                cout << '\'' << exp[i] << "'There's something wrong on the right! mistake:" << exp[i] << exp[i + 1] << endl;
                return 0;
            }
        }
        else if (exp[i] == '(')
        {
            if (exp[i + 1] == ')' || isbinaryoperator(exp[i + 1]))
            {
                cout << '\'' << exp[i] << "'There's something wrong on the right! mistake:" << exp[i] << exp[i + 1] << endl;
                return 0;
            }
        }
        else if (exp[i] == ')')
        {
            if (exp[i + 1] != ')' && !isbinaryoperator(exp[i + 1]))
            {
                cout << '\'' << exp[i] << "'There's something wrong on the right! mistake:" << exp[i] << exp[i + 1] << endl;
                return 0;
            }
        }
        else if (exp[i] == '!')
        {
            if (exp[i + 1] == ')' || isbinaryoperator(exp[i + 1]))
            {
                cout << '\'' << exp[i] << "'There's something wrong on the right! mistake:" << exp[i] << exp[i + 1] << endl;
                return 0;
            }
        }
        else
        {
            if (exp[i + 1] == ')' || isbinaryoperator(exp[i + 1]))
            {
                cout << '\'' << exp[i] << "'There's something wrong on the right! mistake:" << exp[i] << exp[i + 1] << endl;
                return 0;
            }
        }
    }
    return 1;
}
set<char> get_expinfo(const string& exp)
{
    set<char> exp_elem;

    for (string::size_type i = 0; i != exp.size(); ++i)
    {
        if (isupper(exp[i]))
        {
            exp_elem.insert(exp[i]);
        }
    }
    return exp_elem;
}
string infix_to_suffix(const string& exp)
{
    char top;
    stack<char> s;
    string suffix;

    for (string::size_type i = 0; i != exp.size(); ++i)
    {
        if (isupper(exp[i]))
            suffix += exp[i];
        else if (exp[i] == '(')
            s.push(exp[i]);
        else if (isoperator(exp[i]))
        {
            while (1)
            {
                if (s.empty() || s.top() == '(')
                {
                    s.push(exp[i]);
                    break;
                }
                else
                {
                    top = s.top();
                    if (priority(exp[i]) > priority(top) || (exp[i] == '!' && top == '!'))
                    {
                        s.push(exp[i]);
                        break;
                    }
                    else
                    {
                        suffix += s.top();
                        s.pop();
                    }
                }
            }
        }
        else if (exp[i] == ')')
        {
            while (!s.empty() && (top = s.top()) != '(')
            {
                suffix += top;
                s.pop();
            }
            s.pop();
        }
    }
    while (!s.empty())
    {
        suffix += s.top();
        s.pop();
    }
    return suffix;
}
int eval(const string& row, const string& suffix, const set<char>& exp_elem)
{
    char temp = 0;
    int p, q, ret;
    stack<char> s;
    map<char, int> m;
    string::size_type i;

    set<char>::const_iterator pos;
    for (pos = exp_elem.begin(), i = 0; pos != exp_elem.end(); ++pos, ++i)
    {
        m.insert(pair<char, int>(*pos, row[i] - '0'));
    }

    p = q = 0;
    for (i = 0; i != suffix.size(); ++i)
    {
        if (isupper(suffix[i]))
            s.push(m[suffix[i]]);
        else if (isoperator(suffix[i]))
        {
            q = s.top();
            s.pop();

            if (suffix[i] != '!' && !s.empty())
            {
                p = s.top();
                s.pop();
            }

            switch (suffix[i])
            {
            case '!':
                temp = !q;
                break;
            case '&':
                temp = p && q;
                break;
            case '|':
                temp = p || q;
                break;
            case '>':
                temp = !p || q;
                break;
            case '#':
                temp = (!p || q) && (!q || p);
                break;
            }
            s.push(temp);
        }
    }
    ret = s.top();
    s.pop();
    return ret;
}


vector<char> print_table(const string& exp)
{
    int cols, rows, temp;
    string row, suffix;
    set<char> exp_elem;

    exp_elem = get_expinfo(exp);
    for (set<char>::iterator pos = exp_elem.begin(); pos != exp_elem.end(); ++pos)
    {
        cout << '\t' << *pos;
    }
    cout << '\t' << exp << endl;

    suffix = infix_to_suffix(exp);

    cols = exp_elem.size();
    rows = static_cast<int>(pow(2.0, cols));

    bitset<26> bs;
    vector<char> result;
    for (int i = 0; i < rows; ++i)
    {
        bs = i;
#if (_MSC_VER == 1200)  //VC6
        row = bs.to_string();
#else
        row = bs.to_string<char, char_traits<char>, allocator<char> >();
#endif
        row.erase(0, 26 - cols);
        for (int j = 0; j < cols; ++j)
        {
            cout << '\t' << row[j];
        }
        temp = eval(row, suffix, exp_elem);
        result.push_back(temp);
        cout << '\t' << temp << endl;
    }
    return result;
}


int is_tautology(const vector<char>& result)
{
    return result.end() == find(result.begin(), result.end(), 0);
}

int is_contradiction(const vector<char>& result)
{
    return result.end() == find(result.begin(), result.end(), 1);
}


void print_cnf(const vector<char>& result, const set<char>& exp_elem)
{
    if (is_tautology(result))
        return;

    vector<char> elem;
    copy(exp_elem.begin(), exp_elem.end(), back_inserter(elem));

    size_t i, j;
    vector<int> v;
    for (i = 0; i != result.size(); ++i)
    {
        if (result[i] == 0)
            v.push_back(i);
    }

    cout << "Master-taken paradigm\n" << "M(";
    for (i = 0; i != v.size(); ++i)
    {
        if (i < v.size() - 1)
            cout << v[i] << ", ";
        else
            cout << v[i];
    }
    cout << ')' << endl;

    bitset<26> bs;
    string row;
    for (i = 0; i != v.size(); ++i)
    {
        cout << '(';
        bs = v[i];
#if (_MSC_VER == 1200)  //VC6
        row = bs.to_string();
#else
        row = bs.to_string<char, char_traits<char>, allocator<char> >();
#endif
        row.erase(0, 26 - elem.size());
        for (j = 0; j != elem.size(); ++j)
        {
            if (row[j] == '1')
                cout << "┐" << elem[j];
            else
                cout << elem[j];

            if (j < elem.size() - 1)
                cout << "∨";
        }
        if (i < v.size() - 1)
            cout << ")∧";
        else
            cout << ')' << endl;
    }
}

void print_dnf(const vector<char>& result, const set<char>& exp_elem)
{
    if (is_contradiction(result))
        return;

    vector<char> elem;
    copy(exp_elem.begin(), exp_elem.end(), back_inserter(elem));

    size_t i, j;
    vector<int> v;
    for (i = 0; i != result.size(); ++i)
    {
        if (result[i] == 1)
            v.push_back(i);
    }

    cout << "The main paradigm\n" << "M(";
    for (i = 0; i != v.size(); ++i)
    {
        if (i < v.size() - 1)
            cout << v[i] << ", ";
        else
            cout << v[i];
    }
    cout << ')' << endl;

    bitset<26> bs;
    string row;
    for (i = 0; i != v.size(); ++i)
    {
        cout << '(';
        bs = v[i];
#if (_MSC_VER == 1200)  //VC6
        row = bs.to_string();
#else
        row = bs.to_string<char, char_traits<char>, allocator<char> >();
#endif
        row.erase(0, 26 - elem.size());
        for (j = 0; j != elem.size(); ++j)
        {
            if (row[j] == '1')
                cout << elem[j];
            else
                cout << "┐" << elem[j];

            if (j < elem.size() - 1)
                cout << "∧";
        }
        if (i < v.size() - 1)
            cout << ")∨";
        else
            cout << ')' << endl;
    }
}

void cls()
{
#ifdef _WIN32
    system("cls");
#else
    system("clear");
#endif
}

void pause()
{
#ifdef _WIN32
    system("pause");
#else
    system("read -p \"Press any key to continue...\" tmp");
#endif
}

void instruction()
{
    cout << "||====Discrete True Value Table Generator====||\n"
         << "||     '!' Negative junction words     ||\n"
         << "||     '&' Take the conjunction word together     ||\n"
         << "||     '|' Extract the conjunction words   ||\n"
         << "||     '>' Contains conjunction words   ||\n"
         << "||     '#' Equivalent junction word     ||\n"
         << "||     'q' Exit the program       ||\n"
         << "||========================||\n"
         << "Enter an expression:";
}

int main()
{
    string exp;
    set<char> exp_elem;
    vector<char> result;

#ifdef _WIN32
    system("title Discrete mathematical truth tables");
#endif

    while (1)
    {
        cls();
        instruction();
        cin >> exp;

        if (exp == "q")
            break;
        if (!is_valid_expression(exp))
        {
            pause();
            continue;
        }
        exp_elem = get_expinfo(exp);
        result = print_table(exp);
        print_cnf(result, exp_elem);
        print_dnf(result, exp_elem);
        pause();
    }
    return 0;
}