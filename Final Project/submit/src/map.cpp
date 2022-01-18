/* -----------------------------------------------------------------------------------------
   ALS Final project ver_2022/1/18
   
   / complete!
   ----------------------------------------------------------------------------------------- */

#include<iostream>
#include<fstream>
#include<sstream>
#include<vector>
#include<map>
#include<algorithm>
#include<cmath>
#include<math.h>
#include<string>
#include<regex>
#include<climits>
#include<chrono>
#include<thread>
using namespace std;


/* -----     tree node     ----- */
class Node {
	
	public:
		string id;
		int label , operate = -2; // -1_not , 0_+ , 1_*
		Node* first = NULL;
		Node* second = NULL;
	
};


/* -----     variables     ----- */
ifstream blif;
ofstream decomposed , output;
int LUT , out_size , LUTs = 0 , circuit_level;
string FN , func;

vector<string> out;
vector<string> inputs;
vector<string> nodes;

map<string,Node*> find_Node;


/* -----     decomposition     ----- */
void decomposition() {
	
	int name = 1 , Osize , count = 0 , flag , r;
	string temp;
	vector<string> operand;
	vector<int> op;
	
	blif >> temp;	decomposed << temp <<" ";	output << temp <<" "; // .model 
	blif >> temp;	decomposed << temp << endl;	output << temp << endl; // blif name
	
	/* -----     inputs     ----- */
	blif >> temp;	decomposed << temp <<" ";	output << temp <<" ";
	while ( true ) {
		
		blif >> temp;
		if ( temp=="\\" ) continue;
		else if ( temp==".outputs" ) break;
		else {
			
			Node *n = new Node();
			n->id = temp;
			n->label = 0;
			find_Node[temp] = n;
			decomposed << temp << " ";
			output << temp << " ";
			inputs.push_back(temp);
			
		}
		
	}
	decomposed << endl << temp << " ";
	output << endl << temp << " ";
	
	/* -----     outputs     ----- */
	while ( true ) {
		
		blif >> temp;
		if ( temp=="\\" ) continue;
		else if ( temp==".names" ) break;
		else {
			
			decomposed << temp << " ";
			output << temp << " ";
			out.push_back(temp);
			
		}
		
	}
	out_size = out.size();
	decomposed << endl;
	output << endl;
	
	/* -----     construct tree     ----- */
	while ( true ) {
		
		while ( true ) {
			
			blif >> temp;
			if ( temp[0]=='1' or temp[0]=='0' or temp[0]=='-' ) break;
			operand.push_back(temp);
			
		}
		Osize = operand.size()-1;
		
		if ( temp[0]=='-' or temp[1]=='-' ) {
			
			flag = 0;
			for ( int i=0;i<Osize;i++ ) blif >> temp >> temp;
			
		}
		else if ( temp[0]=='1' and temp[1]=='1' ) {
			
			flag = 1;
			blif >> temp >> temp;
			
		}
		else {
			
			flag = 2;
			blif >> temp >> temp;
			
		}
		
		if ( flag==2 ) {
			
			Node *n = new Node();
			n->id = operand[1];
			n->first = find_Node.find(operand[0])->second;
			n->label = n->first->label + 1;
			n->operate = -1;
			find_Node[operand[1]] = n;
			
			decomposed << ".names " << operand[0] << " " << operand[1] << endl;
			decomposed << "0 1" << endl;
			operand.clear();
			
		}
		else {
			
			count = 0;
			while ( true ) {
				
				if ( Osize==2 ) {
					
					count++;
					Osize = operand.size()-1;
					break;
					
				}
				
				count += ( Osize/2 );
				if ( Osize%2==0 ) Osize /= 2;
				else {
					
					Osize /= 2;
					Osize++;
					
				}
				
			}
			
			for ( int i=1;i<count;i++ ) {
				
				op.clear();
				r = 0;
				for ( int j=0;;j++ ) {
					
					for ( int k=0;k<Osize;k++ ) {
						
						if ( find_Node.find(operand[k])->second->label==j ) {
							
							op.push_back(k);
							r++;
							if ( r==2 ) break;
							
						}
						
					}
					if ( r==2 ) break;	
					
				}
				
				decomposed << ".names " << operand[op[0]] << " " << operand[op[1]];
				decomposed << " t_" << to_string(name) << endl; 
				
				Node *n = new Node();
				n->id = "t_" + to_string(name);
				n->first = find_Node.find(operand[op[0]])->second;
				n->second = find_Node.find(operand[op[1]])->second;
				n->label = max( n->first->label , n->second->label ) + 1;
				find_Node[n->id] = n;
				
				operand.erase(operand.begin()+op[0]);
				if ( op[0]<op[1] ) operand.erase(operand.begin()+op[1]-1);
				else operand.erase(operand.begin()+op[1]);
				operand.insert(operand.begin(),"t_"+to_string(name));
				name++;
				
				if ( flag==0 ) {
					
					n->operate = 0;
					decomposed << "1- 1\n-1 1" << endl;
					
				}
				else {
					
					n->operate = 1;
					decomposed << "11 1" << endl;
					
				}
				
				Osize--;
 				
			}
			
			decomposed << ".names " << operand[0] << " " << operand[1] << " " << operand[2] << endl;
			Node *n = new Node();
			n->id = operand[2];
			n->first = find_Node.find(operand[0])->second;
			n->second = find_Node.find(operand[1])->second;
			
			if ( flag==0 ) {
				
				decomposed << "1- 1\n-1 1" << endl;
				n->operate = 0;
				
			}
			else {
				
				decomposed << "11 1" << endl;
				n->operate = 1;
				
			}
			n->label = max( n->first->label , n->second->label ) + 1;
			find_Node[operand[2]] = n;
			operand.clear();

		}
		
		if ( temp==".end" ) {
			
			decomposed << ".end" << endl;
			break;
			
		}
		
	}

	blif.close();
	decomposed.close();
	
}


/* -----     reference:https://stackoverflow.com/questions/52505340/boolean-truth-table-c     ----- */
bool getBit(unsigned int uint, int position) {
	return (uint >> position) & 0x1;
}


void construct_func(string original , vector<string> temp , vector<string> pi) {

	if ( find(temp.begin(),temp.end(),original)!=temp.end() or
	     find(pi.begin(),pi.end(),original)!=pi.end() ) return;

	Node *n = find_Node.find(original)->second;
	if (n->operate==-1) {

		func = regex_replace(func,regex(original),"( ! "+n->first->id+" ) ");
		construct_func(n->first->id,temp,pi);

	}
	else {

		if (n->operate==0) func = regex_replace(func,regex(original),"( "+n->first->id+" || "+n->second->id+" ) ");
		if (n->operate==1) func = regex_replace(func,regex(original),"( "+n->first->id+" * "+n->second->id+" ) ");
		construct_func(n->first->id,temp,pi);
		construct_func(n->second->id,temp,pi);

	}

}


void change_func() {

	istringstream f(func);
	string sub_func , new_string = "";
	vector<string> oper;
	while (f >> sub_func) {

		if (sub_func==")") {

			while (true) {

				string str = oper.back();
				oper.pop_back();
				if (str=="(") break;
				else new_string = new_string + " " + str;

			}

		}
		else {

			if (sub_func=="(" or sub_func=="*" or sub_func=="||" or sub_func=="!") oper.push_back(sub_func);
			else new_string = new_string + " " + sub_func;

		}

	}
	func = new_string;

}


void cal_level(vector<string> v) {

	int n_id = -1 , max_level;
	string temp;
	map<string,int> mapping;
	vector<string> funcs[out_size];
	for (auto i: inputs) mapping[i] = 1;

	for (auto s: v) {
		
		istringstream ss(s);
		while (ss >> temp);
		for (auto o: out) if (temp==o) n_id++;
		funcs[n_id].push_back(s);
		
	}

	for (auto f: funcs) {
		
		for ( auto sub_f = f.rbegin(); sub_f!=f.rend() ; ++sub_f ) {
			
			max_level = 0;
			vector<string> cal;
			istringstream s(sub_f->c_str());
			while (s >> temp) cal.push_back(temp);
			cal.pop_back();
			for (auto c: cal) {

				if ( mapping.find(c)->second>10000 ) mapping.find(c)->second = 0;
				max_level = max(mapping.find(c)->second,max_level);

			}
			mapping[temp] = max_level + 1;
			

		}

	}
	// for (auto m: mapping) cout<<m.first<<" "<<m.second<<endl;
	max_level = 0;
	for (auto o: out) max_level = max(max_level , mapping.find(o)->second);
	circuit_level = max_level;

}


void technology_mapping() {
	
	vector<string> check;
	vector<string> temp_out = out;
	
	while (temp_out.size()!=0) {
		
		nodes.push_back(temp_out[0]);
		temp_out.erase(temp_out.begin());
		
		while (nodes.size()!=0) {
			
			int id = 0;
			func = nodes[0];
			map<string,int> bool_cal_node;
			vector<string> temp;
			vector<string> pi;
			Node *n = find_Node.find(nodes[0])->second;
			temp.push_back(nodes[0]);
			
			while (true) {
				
				if ((temp.size()+pi.size())==LUT or temp.size()==0) break;
				if (n->first!=NULL) {

					if (find(inputs.begin(),inputs.end(),n->first->id)==inputs.end()) temp.push_back(n->first->id);
					else pi.push_back(n->first->id);
					if (n->second!=NULL) {

						if (find(inputs.begin(),inputs.end(),n->second->id)==inputs.end()) temp.push_back(n->second->id);
						else pi.push_back(n->second->id);

					}
					temp.erase(temp.begin());

				}
				else {

					pi.push_back(temp[0]);
					temp.erase(temp.begin());

				}
				n = find_Node.find(temp[0])->second;

			}
			sort(temp.begin(),temp.end());
			temp.erase(unique(temp.begin(),temp.end()),temp.end());
			sort(pi.begin(),pi.end());
			pi.erase(unique(pi.begin(),pi.end()),pi.end());

			string checking="";
			for (int i=0;i<pi.size();i++) {

				checking = checking + " " + pi[i];
				bool_cal_node[pi[i]] = i;

			}
			for (int i=0;i<temp.size();i++) {

				checking = checking + " " + temp[i];
				bool_cal_node[temp[i]] = pi.size() + i;
				nodes.push_back(temp[i]);

			}
			checking = checking + " " + nodes[0];
			construct_func(nodes[0],temp,pi);

			if (find(check.begin(),check.end(),checking)==check.end()) {

				check.push_back(checking);
				output << ".names" << checking << endl;
				LUTs++;

				int size = temp.size()+pi.size();
				bool opr[size];
				change_func();
				for (unsigned int i=0;i<pow(2,size);i++) {

					int opr_num;
					bool a , b;
					string str , o = "";
					vector<bool> opd;

					for (int j=0;j<size;j++) {

						opr[j] = getBit(i,j);
						o = o + to_string(opr[j]);
						// cout << opr[j] << " ";

					}
					istringstream f(func);
					while (f >> str) {
						
						if (str=="||") {

							a = opd.back();
							opd.pop_back();
							b = opd.back();
							opd.pop_back();
							opd.push_back(a||b);
							
						}
						else if (str=="*") {

							a = opd.back();
							opd.pop_back();
							b = opd.back();
							opd.pop_back();
							opd.push_back(a*b);
							
						}
						else if (str=="!") {

							a = opd.back();
							opd.pop_back();
							opd.push_back(!a);
							
						}
						else {

							opr_num = bool_cal_node.find(str)->second;
							opd.push_back(opr[opr_num]);

						}

					}

					if (opd[0]==1) {

						o = o + " 1";
						output << o << endl;

					}
					// cout << opd[0];
					// cout << endl;
				}

				// cout << ".names" << checking << endl;
				// cout << func << endl << endl;

			}

			nodes.erase(nodes.begin());
			temp.clear();
			pi.clear();
			// for (auto p: nodes) cout<<p<<endl;
			// std::this_thread::sleep_for(std::chrono::seconds(2));

		}

		// cout<<"==============="<<endl;

	}
	cal_level(check);
	output << ".end";
	output.close();

}


/* -----     print nodes     ----- */
void print_Nodes() {
	
	std::map<std::string,Node*>::iterator iter;
	for(iter=find_Node.begin();iter!=find_Node.end();iter++){
		
		cout << "Node : " << iter->second->id << endl;
		cout << "operate : " << iter->second->operate << endl;
		cout << "Label : " << iter->second->label << endl;
		if ( iter->second->first!=NULL )
			cout << "first : " << iter->second->first->id << endl;
		if ( iter->second->second!=NULL )
			cout << "second : " << iter->second->second->id << endl;
		cout << endl;
		
	}
	
}


int main ( int argc , char *argv[] ) {
	
	LUT = stoi(argv[2]);
	FN = argv[3];
	FN = FN.substr(8,FN.length()-13);
	blif.open(argv[3]);
	decomposed.open("../decomposed/"+FN+".blif");
	output.open(argv[4]);
	
	decomposition();
	technology_mapping();
	// print_Nodes();

	cout << "The circuit level is " << circuit_level << ".\n";
	cout << "The number of LUTs is " << LUTs << ".\n";
	
	return 0;
	
}

